* Todo: `project_branch_default_{pull,push}` require some attention when a remote is updated. For example, if "origin" is edited to point to another project id then the default push and pull rows that reference "origin" should be updated. Unfortunately, the default push/pull tables have a foreign key reference on `project_remote_alias` that includes the remote project id and remote host in order to ensure they agree.

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

The project branch table. Each row corresponds to a different local branch of a local project (either created locally, or pulled from somewhere).

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
  remote_branch_id text not null,

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

### `project_mapping` / `project_branch_mapping`

An association between local and remote projects and branches. Each local project is associated with at most 1 remote project on each host (e.g. `share.unison-lang.org`); separately, each local branch is associated with at most 1 remote branch on each host.

This association exists only to streamline workflows that fit the common case of having a single associated remote project/branch. Ultimately this association will be editable by the user, in case our heuristics (like associating a local project to a remote project on first pull or first push) aren't what they want.

Note: it would be unusual, but also fine, for a local project and a branch of that project to be associated with two different remote projects.

```sql
create table project_mapping (
  local_project_id uuid not null references project (id),

  -- The remote project
  remote_project_id text not null,
  remote_host text not null,

  -- A local project can only be associated with one remote project per host
  primary key (local_project_id, remote_host)

  foreign key (remote_project_id, remote_host)
    references remote_project (id, host)
    on delete cascade
)
without rowid;
```

```sql
create table project_branch_mapping (
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

### Notes from 23/01/11

Q: how do we maintain association between contributor branches (a copy of @unison/base:main) and the original repo, so that we can have a nice low friction way to open pull requests?

Q: On Share, we need to be able to separately list contributor branches and branches that you own. (Like a fork of @unison/base:main vs being the owner of @unison/base). How do we do that?

* On Share, do we have some schema or PostgreSQL to support this? Seems like yes: ucm will not treat contributor branches as special, but on Share, it'll do something.
* On ucm side, there'll something that happens when creating a branch locally which is intended to be a contributor branch?

Q: when I create a contributor branch of @unison/base:main, and I push that to Share, does it use the same project_id for that contributor branch, or does it make up a new one?

  1. Option 1: just use the same project id.
  2. Option 2: just make up a new one.

  If we do 2), then Share needs to maintain a mapping so it can suggest a good default PR target.

  Regardless of the choice, we need an extra bit of information to identify contributor branches,
  so that when you're viewing your homepage, you don't see a fork of base when you've just forked
  one of its branches. We don't want the GitHub experience of having all these random project forks
  in your homepage just because you wanted to do a PR one time. And the current UI supports this,
  putting contributor branches in a separate spot.

  Related Q: do we make up a new remote branch_id?
  A: Yes. The fork of the branch gets a new identity.

Q: what additional information is passed to push/pull to support projects?

---

# UCM

Projects and their branches will be stored in the root namespace:

```
.__projects.<local-project-id>.branches.<local-branch-id>
```

## Conceptual plumbing commands

### create-project

Create a new local project.

```elm
create-project :
  { name : String
  }
  -> LocalProjectId
```

Example:

```elm
create-project
  { name = "parser"
  }

"2ea56758"
```

```sql
-- Leaving null:
--   default_remote_pull
--   default_remote_push
INSERT INTO projects (id, name)
VALUES ('2ea56758', 'parser');
```

### create-remote-branch

Create a new project branch on a server, optionally associated with a parent branch on that server.

```elm
create-remote-branch :
  { server : Url
  , project-id : RemoteProjectId
  , parent :
      Maybe
        { project-id : RemoteProjectId
        , branch-id : RemoteBranchId
        }
  }
  -> RemoteBranchId
```

Example:

```elm
create-remote-branch
  { server = "https://share.unison-lang.org"
  , project-id = "de4644ac"
  , parent =
      Just
        { project-id = "bf070d5e"
        , branch-id = "76228b2c"
        }
  }

"2183b77f"
```

### create-remote-project

Create a new project on a server.

```elm
create-remote-project :
  { server : User
  , name : String
  }
  -> RemoteProjectId
```

Example:

```elm
create-remote-project
  { server = "https://share.unison-lang.org"
  , name = "@arya/lens"
  }

"2183b77f"
```

### get-branch-causal-hash

Ask a server for a branch's causal hash.

```elm
get-branch-causal-hash :
  { server : Url
  , branch-id : RemoteBranchId
  }
  -> CausalHash
```

Example:

```elm
get-branch-causal-hash
  { server = "https://share.unison-lang.org"
  , branch-id = "4d6e8803"
  }

"dd2781f4"
```

### get-branch-id

Ask a server to resolve a branch name to remote branch id.

```elm
get-branch-id :
  { server : Url
  , project-id : RemoteProjectId
  , branch : String
  }
  -> RemoteBranchId
```

Example:

```elm
get-branch-id
  { server = "https://share.unison-lang.org"
  , project-id = "10f43936"
  , branch = "main"
  }

"4d6e8803"
```

### get-project-id

Ask a server to resolve a project name to remote project id.

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

a) Are you the owner of this project or are you pushing a contributor branch? (Share needs this so the UI can show your contributor branches in a separate location than projects/branches that you "own")
b) If a contributor branch, what's the default merge target? (Shares needs this to present a nice UI)

All branches have a project_id.
Some branches have a parent (project_id, branch_id)

A contributor branch is one whose parent project_id
differs from its project id.

In creating the remote branch, we will do a lookup in the branch_parent table, join this with the project_branch_id_mappings (to see if there's a corresponding remote branch), and if so, include this parent information in the request sent to Share.

This way, when creating a branch on Share, the Share API is told (optionally) of a parent branch, which is used to select a default merge target and to determine if the branch is a contributor branch or not.

After that, it's just a normal push.

## What happens when I `pull`?

This is the same as now.

We're not using `pull` to checkout new projects. Use `project.switch` for that.

# Random historical Q&A that seems resolved?

What happens when I push a project branch? (You don't "push a project", you create the project, then push individual branches. The user can if they want push multiple branches, but no special command needed for now.)

---

Question: when I create a contributor branch, does it get a new project_id and branch_id?

1. We considered: it gets the same project_id, and a new branch_id
  If if do this, then (username, project_id) becomes the primary key on Share.
2. OR: it gets a new project_id
- We reasoned that if it get a project_id, Share will maintain an association between the
  branch_id and the (project_id, branch_id) which it is a fork of (and which is a default merge target).

Seems like either could work, but we decided to go with 2) for now, unless something comes up
later that makes us seriously reconsider.

-# Unresolved questions

### How can we support undeleting a branch?

---

Default merge target location? (at level of project, and also overrideable for individual branches). Specify somehow in SQL.
- Do we add that as a column to projects table?

Default push location? (at level of project, and also overrideable for individual branches). Specify somehow

(local id, remote id, server name) relation


____

1. on a plane, create a project within ucm (so you don't create the project on Share then pull it down)

> project.create lens

3. maybe do some work
4. push it to share

`.__projects.<origguid>.branches.main> push`

How do I work on this project now?

5. Is it: project.switch lens

    Answer 1: it's just the same as `cd .__projects.<origguid>.branches.main`

   Is it: project.switch @arya/lens

    Answer 1: ask share for @arya/lens and save it to `.__projects.<otherguid>`

### What about UUIDs we generate that begin with a number? Is it ok to make child namespaces out of those?

Example: `.__projects.12345.branches.67890`


----
Scenario 1:

> project.create lens
> project.switch lens
...lens...> push //creates @arya/lens?
> project.switch @arya/lens

Q: I have two projects, "lens" and "@arya/lens" that are totally independent?

Scenario 2:

I create @arya/lens on the website
> project.switch @arya/lens

Q: I just have one project locally?

----



----

Consequences of single uuid vs local and remote uuids

If we want local projects to eventually support multiple remotes, then we will eventually have a many-to-many relationship between local projects and remote projects.

If there is a single global uuid for a project then what happens if someone clones an open-source project and creates a private remote? The open-source project has some uuid that is supposed to globally identify it, but we don't want the new private project to share this identifier, it is meant to be a new distinct project.

    If the open and closed projects share the same UUID, that doesn't work because they're meant to be distinct.
    If they have different UUIDs, then they (we presume) can't both be remotes for a given local project.

"What if they could?" "That is the purpose of the mapping."

----

Suppose we have a mapping from local uuids and remote uuids.
(localuuid, remoteuuid, server)
