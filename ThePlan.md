* Todo: `project_branch_default_{pull,push}` require some attention when a remote is updated. For example, if "origin"
is edited to point to another project id then the default push and pull rows that reference "origin" should be updated.
Unfortunately, the default push/pull tables have a foreign key reference on `project_remote_alias` that includes the
remote project id and remote host in order to ensure they agree.

# Sql tables / queries

### `project`

The project table. Each row corresponds to a different local project.

```sql
create table project (
  id uuid primary key,

  -- The local name of the project, e.g. "lens" or "@arya/lens"
  name text unique not null
)
without rowid;
```

### `project_branch`

The project branch table. Each row corresponds to a different local branch of a local project (either created locally,
or pulled from somewhere).

```sql
create table project_branch (
  project_id uuid not null references project (id),
  branch_id uuid not null,

  -- The local name of the branch, e.g. "main"
  name text not null,

  primary key (project_id, branch_id),

  -- A project can't give the same name to two different branches
  unique (project_id, name)
)
without rowid;
```

### `project_branch_parent`

```sql
create table project_branch_parent (
  project_id uuid not null references project (id),
  branch_id uuid not null,
  parent_branch_id uuid not null,

  primary key (project_id, branch_id),

  foreign key (project_id, branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,

  foreign key (project_id, parent_branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade
)
without rowid;
```

### `project_branch_default_pull` / `project_branch_default_push`

Defaults for where to push/pull when not specified.

```sql
create table project_branch_default_pull (
  local_project_id uuid not null references project (id),
  local_branch_id uuid not null,

  -- Sum type: either we pull from a named alias (e.g. "origin"), or an unnamed remote branch.
  named_remote_name text null,
  named_remote_project_id text null,
  named_remote_host text null,
  --
  unnamed_remote_project_id text null,
  unnamed_remote_host text null,

  constraint valid_sum check
    ( (named_remote_name is not null
      and named_remote_project_id is not null
      and named_remote_host is not null
      and unnamed_remote_project_id is null
      and unnamed_remote_host is null)
    or
      (named_remote_name is null
      and named_remote_project_id is null
      and named_remote_host is null
      and unnamed_remote_project_id is not null
      and unnamed_remote_host is not null)
    ),

  -- This is for integrity purposes. TODO better comment
  -- TODO comment why nullable. hint to self: metadata for topic2 after pushing topic1
  remote_branch_id text null,

  primary key (local_project_id, local_branch_id),

  foreign key (local_project_id, local_branch_id)
    references project_branch (project_id, id),

  -- Even though we could have a foreign key to just the primary key of
  -- project_remote_alias, which is (local_project_id, remote_name), we include
  -- `named_remote_project_id` and `named_remote_host` so that those two columns
  -- must agree.
  foreign key (local_project_id, named_remote_project_id, named_remote_host, named_remote_name)
    references project_remote_alias (local_project_id, remote_project_id, remote_host, remote_name)
    on delete cascade,

  foreign key (named_remote_project_id, remote_branch_id, named_remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade,

  foreign key (unnamed_remote_project_id, remote_branch_id, unnamed_remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade
)
without rowid;

-- Same as above
create table project_branch_default_push ...
```

### `project_branch_remote_mapping`

An association between local and remote project branches. Each local branch project is associated with at most 1 remote
branch on each host.

This association exists only to streamline workflows that fit the common case of having a single associated remote
branch. Ultimately this association will be editable by the user, in case our heuristics (like associating a local
branch to a remote project on first pull or first push) aren't what they want.

```sql
create table project_branch_remote_mapping (
  local_project_id uuid not null references project (id),
  local_branch_id uuid not null,

  -- The remote branch
  remote_project_id text not null,
  remote_branch_id text not null,
  remote_host text not null,

  -- A local branch can only be associated with one remote branch per host
  primary key (local_project_id, local_branch_id, remote_host),

  foreign key (local_project_id, local_branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,

  foreign key (remote_project_id, remote_branch_id, remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade
)
without rowid;
```

### `project_remote_alias`

Convenience table: short names for associated remote projects.

For example, in my local project `"@unison/base"`, I might have two remotes:

- `"origin"`, which refers to the project `@unison/base` on `https://share.unison-lang.org`
- `"mine"`, which refers to the project `@arya/@unison/base` on `https://share.unison-lang.org`
  - Note: `@arya/@unison/base` refers to the project named `"@unison/base"`, created by user `@arya`.

These rows should not be edited in-place, but rather dropped and created anew, so that the deletes cascade to any associated default push/pull tables. Nice-to-have: allowing edits, and then resolving all of the new project's branch ids, trying to retain the defaults.

```sql
create table project_remote_alias (
  local_project_id uuid not null references project (id),

  -- The remote project we are giving a short name to
  remote_project_id text not null,
  remote_host text not null,

  -- The short name for this remote project, e.g. "origin"
  remote_name text not null,

  -- Can't give the same name (e.g. "origin") to two different remote projects
  primary key (local_project_id, remote_name),

  -- So `project_branch_default_{pull,push}` can have a foreign key here
  unique (local_project_id, remote_project_id, remote_host, remote_name),

  foreign key (remote_project_id, remote_host)
    references remote_project (id, host)
    on delete cascade,
)
without rowid;
```

### `remote_project` / `remote_project_branch`

Remote projects/branches that we know about. This data could be completely wiped out at any time; it just serves as a cache for displaying the previously-fetched names.

For example, you might have a local project called `"lens"` that's tracking some remote `"@arya/lens"` (by UUID), which we want to display (wherever it's requested) as the nice name `"@arya/lens"`.

But Arya could go rename his `"@arya/lens"` to `"@arya/coollens"` on the server, so we want to (in some automated/regularly-occurring way, e.g. whenever pushing/pulling) update our local name to match.

```sql
create table remote_project (
  -- Remote project id
  id text not null,
  host text not null,

  -- The last known (remote) name we know about, e.g. "@arya/lens"
  name text not null,

  primary key (id, host)
)
without rowid;
```

```sql
create table remote_project_branch (
  -- Remote project/branch ids
  project_id text not null,
  branch_id text not null,
  host text not null,

  -- The last known (remote) name we know about, e.g. "main"
  name text not null,

  primary key (project_id, branch_id, host),

  foreign key (project_id, host) references remote_project (id, host)
)
without rowid;
```

---

Below this line is a mess

---

# UCM

Projects and their branches will be stored in the root namespace:

```
.__projects.<local-project-id>.branches.<local-branch-id>
```

## API calls

### create-branch

Create a new project branch on a server with an optional merge target.

```elm
create-branch :
  { server : Url
  , project-id : RemoteProjectId
  , branch-name : String
  , merge-target :
      Maybe
        { project-id : RemoteProjectId
        , branch-id : RemoteBranchId
        }
  , causal-hash : CausalHash
  }
  -> RemoteBranchId
```

Example:

```elm
create-branch
  { server = "https://share.unison-lang.org"
  , project-id = "de4644ac"
  , branch-name = "topic"
  , merge-target =
      Just
        { project-id = "bf070d5e"
        , branch-id = "76228b2c"
        }
  , causal-hash = "whatever"
  }

"2183b77f"
```

### create-project

Create a new project on a server.

Fixme: there's already an API up, adjust this to match.

```elm
create-project :
  { server : Url
  , name : String
  }
  -> RemoteProjectId
```

Example:

```elm
create-project
  { server = "https://share.unison-lang.org"
  , name = "@arya/lens"
  }

"2183b77f"
```

### get-project-branch-causal-hash

Ask a server for a project branch's causal hash.

FIXME: rework this into get-branch-by-id/get-branch-by-name that return more info (like branch name)

```elm
get-project-branch-causal-hash :
  { server : Url
  , project-id : RemoteProjectId
  , branch-id : RemoteBranchId
  }
  -> CausalHash
```

Example:

```elm
get-project-branch-causal-hash
  { server = "https://share.unison-lang.org"
  , project-id = "10f43936"
  , branch-id = "4d6e8803"
  }

"dd2781f4"
```

### get-project-branch-id

Ask a server to resolve a branch name to remote branch id.

```elm
get-projectbranch-id :
  { server : Url
  , project-id : RemoteProjectId
  , branch-name : String
  }
  -> RemoteBranchId
```

Example:

```elm
get-project-branch-id
  { server = "https://share.unison-lang.org"
  , project-id = "10f43936"
  , branch-name = "main"
  }

"4d6e8803"
```

### get-project-id

Ask a server to resolve a project name to remote project id.

FIXME: similarly, rework as get-project-by-id / get-project-by-name

```elm
get-project-id :
  { server : Url
  , project : String
  }
  -> RemoteProjectId
```

Example:

```elm
get-project-id
  { server = "https://share.unison-lang.org"
  , project = "@arya/parser"
  }

"10f43936"
```

### push-project-branch

FIXME

- project id
- branch name
- next head (already pushed)
- <Optional> expected head hash (if provided, do a “force-push-with-lease” style compare-and-swap)
- Don’t support paths, always update the root of the branch.

---

Old chicken scratch of questionable utility

---

# User-facing commands

## `project.create lens`

```
> project.create lens
```

- Make a new local project with a `"main"` branch.

  ```sql
  INSERT INTO projects (id, name)
  VALUES ('c1436c7c', 'lens');

  INSERT INTO project_branch (id, project_id, name)
  VALUES ('658af2af', 'c1436c7c', 'main');
  ```

- Create and store the following namespace:

  ```
  .__projects.c1436c7c.branches.658af2af.README
  ```

- Cd into `.__projects.c1436c7c.branches.658af2af`

Error conditions:

- Trying to create a project whose name already exists.

## `project.switch @unison/base`

Possible states:

1. The project doesn't exist locally yet.
2. The project already exists locally (just based on the existence of `@unison/base` as name your projects table)

### What happens when I `project.switch` to a name that doesn't exist locally?

1. Ask Share to resolve `project="@unison/base" branch="main"` to UUIDs. Hit share for that info. Also get causal hash.

2. Make new UUID for the project.

  ```sql
  INSERT INTO projects (id, name)
  VALUES (<local-project-id>, '@unison/base');

  -- default pull is where you just pulled from, and it
  -- gets the remote_name 'origin' by default
  -- FIXME this is old
  INSERT INTO project_default_pull (project_id, remote_name)
  VALUES (<local-project-id>, 'origin');

  INSERT INTO project_branch (id, project_id, name)
  VALUES
    ( <local-branch-id>
    , <local-project-id>
    , 'main'
    );

  INSERT INTO branch_default_pull (project_id, branch_id, remote_name, remote_branch_id)
  VALUES (<local-project-id>, <local-branch-id>, 'origin', <remote-branch-id>);

  INSERT INTO project_id_mappings
    ( local_id
    , remote_id
    , remote_host
    )
  VALUES
    ( <local-project-id>
    , <remote-project-id>
    , 'https://share.unison-lang.org'
    );

  INSERT INTO project_branch_id_mappings
    ( local_id
    , remote_id
    , remote_host
    )
  VALUES
    ( <local-branch-id>
    , <remote-branch-id>
    , 'https://share.unison-lang.org'
    );

  INSERT INTO project_remote
    ( local_project_id
    , remote_project_id
    , host
    , name
    )
  VALUES
    ( <local-project-id>
    , <remote-project-id>
    , 'https://share.unison-lang.org'
    , 'origin'
    );
```


If the `default_remote_push` field is null and a push command is issued then ucm will default to creating a new remote project and set the branch as a contributor branch if the project branch's "cloned from" fields are non-null (`remote_project_parent_id`, `remote_branch_parent_id`, `remote_host`).


3. Pull the branch (by causal hash) into `.__projects.<local-project-id>.branches.<local-branch-id>`

4. Cd into `.__projects.<local-project-id>.branches.<local-branch-id>`.

Q: Do we actually want to change \<main> to some automatically-generated branch name, e.g. `ludicrous-beaver`? (To discourage people from modifying main branch)
A: No. Maybe they just want to view `main`, in which case there's no reason to change branch names. It's only when `update` that we might want a new branch name.


### What happens when I `project.switch` to a name that does exist locally?

```
.> project.switch my-local-lib
```

- Look up id of project `my-local-lib`.
- Look up id of branch `main`.
- `cd` `.__projects.<project-id>.branches.<branch-id>`

BONUS: remember the last branch you were working on in that project, and switch to that instead of `main`.

BONUS:

```sql
-- table to keep track of the last branch you
-- were working on in each project, so that
-- project.switch mylib will take you to that
-- branch instead of always assuming 'main'
create table last_branch (
 branch_id uuid not null references project_branch(id),
 project_id uuid primary key references project(id)
);
```

## `project.create-branch main myfeature`

Precondition: you have to be in a project.

```
@me/distributed:main> project.create-branch main myfeature

  DONE

@me/distributed:myfeature>
```

We inherit the `remote_project_{parent, branch_parent}_id` from the branch we're forking from (in this case 'main').

FIXME: Come back to this after push and pull

```sql
INSERT INTO project_branch
  ( id
  , project_id
  , name
  , remote_project_parent_id
  , remote_branch_parent_id
  , remote_host
  , default_remote_pull
  , default_branch_pull
  , default_remote_push
  , default_branch_push
  )
VALUES
  ( <local-branch-id>
  , <local-project-id>
  , 'myfeature'
  , <inherited>
  , <inherited>
  , <inherited>
  , null -- it'll default push to your account
  , null
  , null
  , null
  );
```

Does a `fork` of the `main` namespace, and then `cd` into that.

```
@me/distributed:myfeature>
```

## What happens when I `fork`?

Nothing different than today. Use `project.create-branch` instead.

## `push`

Just do a push of current branch to the default push location,
or to the location provided as an argument to `push`.

```
@me/blah:myfeature> push @unison/base:ooga
```

If default push location for this branch isn't set, just set it to where you just pushed.

No branches or anything being created. Nothing fancy.

## `push.create`

This _is_ creating projects and/or branches remotely, so Share needs to know:

a) Are you the owner of this project or are you pushing a contributor branch? (Share needs this so the UI can show your
contributor branches in a separate location than projects/branches that you "own")
b) If a contributor branch, what's the default merge target? (Shares needs this to present a nice UI)

All branches have a project_id.
Some branches have a parent (project_id, branch_id)

A contributor branch is one whose parent project_id
differs from its project id.

In creating the remote branch, we will do a lookup in the branch_parent table, join this with the
project_branch_id_mappings (to see if there's a corresponding remote branch), and if so, include this parent
information in the request sent to Share.

This way, when creating a branch on Share, the Share API is told (optionally) of a parent branch, which is used to
select a default merge target and to determine if the branch is a contributor branch or not.

After that, it's just a normal push.

## What happens when I `pull`?

This is the same as now.

We're not using `pull` to checkout new projects. Use `project.switch` for that.

---

Question: when I create a contributor branch, does it get a new project_id and branch_id?

1. We considered: it gets the same project_id, and a new branch_id
  If if do this, then (username, project_id) becomes the primary key on Share.
2. OR: it gets a new project_id
- We reasoned that if it get a project_id, Share will maintain an association between the
  branch_id and the (project_id, branch_id) which it is a fork of (and which is a default merge target).

Seems like either could work, but we decided to go with 2) for now, unless something comes up
later that makes us seriously reconsider.

# Some questions

### How can we support undeleting a branch?

---

### What about UUIDs we generate that begin with a number? Is it ok to make child namespaces out of those?

Example: `.__projects.12345.branches.67890`

Answer: could prefix UUIDs with an underscore
Answer: could relax namespace parsing
