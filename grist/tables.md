# Grist Internal Tables Schema

This document describes the internal `_grist_*` tables exposed by the Grist
document data engine.

> Source: Grist Core, `app/common/schema.ts`
>
> Current schema version: 46
>
> Last checked: 2026-08-20

## Important distinction

Grist has two categories of internal tables:

- `_grist_*` — document-engine metadata tables. These are the tables relevant
  to document introspection through APIs such as `GristDocAPI.fetchTable()`.
- `_gristsys_*` — storage/server internals used by Grist's document storage
  layer.

The schema documented here is the `_grist_*` document-engine schema.

---

# Internal tables

## `_grist_DocInfo`

Document-wide metadata.

```text
docId             Text
peers             Text
basketId          Text
schemaVersion     Int
timezone          Text
documentSettings  Text
```

---

## `_grist_Tables`

One row for each document table.

```text
tableId                  Text
primaryViewId            Ref:_grist_Views
summarySourceTable       Ref:_grist_Tables
onDemand                 Bool
rawViewSectionRef        Ref:_grist_Views_section
recordCardViewSectionRef Ref:_grist_Views_section
```

The `tableId` is the actual Grist table identifier, such as `Table1`.

---

## `_grist_Tables_column`

Column definitions for document tables.

```text
parentId              Ref:_grist_Tables
parentPos             PositionNumber
colId                 Text
type                  Text
widgetOptions         Text
isFormula             Bool
formula               Text
label                 Text
description           Text
untieColIdFromLabel   Bool
summarySourceCol      Ref:_grist_Tables_column
displayCol            Ref:_grist_Tables_column
visibleCol             Ref:_grist_Tables_column
rules                 RefList:_grist_Tables_column
reverseCol            Ref:_grist_Tables_column
recalcWhen            Int
recalcDeps            RefList:_grist_Tables_column
```

This is one of the most useful tables for programmatic schema introspection.

Important fields include:

- `parentId` — the table containing the column.
- `colId` — the column identifier.
- `type` — the Grist column type.
- `isFormula` — whether the column is a formula.
- `formula` — the formula text.
- `label` — the user-visible column label.
- `widgetOptions` — widget configuration.
- `displayCol` — display-column relationship.
- `visibleCol` — visible-column relationship.

---

## `_grist_Imports`

Import configuration.

```text
tableRef          Ref:_grist_Tables
origFileName      Text
parseFormula      Text
delimiter         Text
doublequote       Bool
escapechar        Text
quotechar         Text
skipinitialspace  Bool
encoding          Text
hasHeaders        Bool
```

---

## `_grist_External_database`

External database configuration.

```text
host      Text
port      Int
username  Text
dialect   Text
database  Text
storage   Text
```

---

## `_grist_External_table`

Mapping between a Grist table and an external database table.

```text
tableRef     Ref:_grist_Tables
databaseRef  Ref:_grist_External_database
tableName    Text
```

---

## `_grist_TableViews`

Associates tables with views.

```text
tableRef  Ref:_grist_Tables
viewRef   Ref:_grist_Views
```

---

## `_grist_TabItems`

Associates tables with tab items.

```text
tableRef  Ref:_grist_Tables
viewRef   Ref:_grist_Views
```

---

## `_grist_TabBar`

Tab ordering.

```text
viewRef  Ref:_grist_Views
tabPos   PositionNumber
```

---

## `_grist_Pages`

Page hierarchy and page configuration.

```text
viewRef       Ref:_grist_Views
indentation   Int
pagePos       PositionNumber
shareRef      Ref:_grist_Shares
options       Text
```

---

## `_grist_Views`

Document views.

```text
name        Text
type        Text
layoutSpec  Text
```

---

## `_grist_Views_section`

Sections/widgets belonging to views.

```text
tableRef             Ref:_grist_Tables
parentId             Ref:_grist_Views
parentKey            Text
title                Text
description          Text
defaultWidth         Int
borderWidth          Int
theme                Text
options              Text
chartType            Text
layoutSpec           Text
filterSpec           Text
sortColRefs          Text
linkSrcSectionRef    Ref:_grist_Views_section
linkSrcColRef        Ref:_grist_Tables_column
linkTargetColRef     Ref:_grist_Tables_column
embedId              Text
rules                RefList:_grist_Tables_column
shareOptions         Text
```

This table contains metadata for the widgets/sections making up a view.

---

## `_grist_Views_section_field`

Fields/columns displayed by a view section.

```text
parentId       Ref:_grist_Views_section
parentPos      PositionNumber
colRef         Ref:_grist_Tables_column
width          Int
widgetOptions  Text
displayCol     Ref:_grist_Tables_column
visibleCol     Ref:_grist_Tables_column
filter         Text
rules          RefList:_grist_Tables_column
```

---

## `_grist_Validations`

Column/table validation definitions.

```text
formula   Text
name      Text
tableRef  Int
```

---

## `_grist_REPL_Hist`

REPL/code history.

```text
code        Text
outputText  Text
errorText   Text
```

---

## `_grist_Attachments`

Attachment metadata.

```text
fileIdent      Text
fileName       Text
fileType       Text
fileSize       Int
fileExt        Text
imageHeight    Int
imageWidth     Int
timeDeleted    DateTime
timeUploaded   DateTime
```

---

## `_grist_Triggers`

Automation trigger definitions.

```text
tableRef           Ref:_grist_Tables
eventTypes         ChoiceList
isReadyColRef      Ref:_grist_Tables_column
actions            Text
label              Text
memo               Text
enabled             Bool
watchedColRefList  RefList:_grist_Tables_column
options             Text
condition           Text
```

---

## `_grist_ACLRules`

Access-control rules.

```text
resource          Ref:_grist_ACLResources
permissions       Int
principals        Text
aclFormula        Text
aclColumn         Ref:_grist_Tables_column
aclFormulaParsed  Text
permissionsText   Text
rulePos           PositionNumber
userAttributes    Text
memo              Text
```

---

## `_grist_ACLResources`

ACL resources.

```text
tableId  Text
colIds   Text
```

---

## `_grist_ACLPrincipals`

ACL users/groups/principals.

```text
type        Text
userEmail   Text
userName    Text
groupName   Text
instanceId  Text
```

---

## `_grist_ACLMemberships`

ACL group membership relationships.

```text
parent  Ref:_grist_ACLPrincipals
child   Ref:_grist_ACLPrincipals
```

---

## `_grist_Filters`

View-section filters.

```text
viewSectionRef  Ref:_grist_Views_section
colRef          Ref:_grist_Tables_column
filter          Text
pinned          Bool
```

---

## `_grist_Cells`

Cell/thread/comment metadata.

```text
tableRef     Ref:_grist_Tables
colRef       Ref:_grist_Tables_column
rowId        Int
root         Bool
parentId     Ref:_grist_Cells
type         Int
content      Text
userRef      Text
timeCreated  DateTime
timeUpdated  DateTime
resolved     Bool
```

---

## `_grist_Shares`

Share-link metadata.

```text
linkId       Text
options      Text
label        Text
description  Text
```

---

# Schema relationships

The most useful relationships for document introspection are:

```text
_grist_Tables
    │
    ├── _grist_Tables_column
    │
    ├── _grist_Views_section
    │       │
    │       └── _grist_Views_section_field
    │
    └── _grist_Views
            │
            ├── _grist_TabBar
            ├── _grist_TabItems
            └── _grist_Pages
```

More explicitly:

```text
Table
  │
  ├── Columns
  │     └── _grist_Tables_column.parentId
  │
  ├── Primary View
  │     └── _grist_Tables.primaryViewId
  │
  └── View Sections
        │
        └── Section Fields
              └── Column
```

---

# Fetching the tables from a plugin

The Grist plugin API exposes `fetchTable()` on `GristDocAPI`.

For example:

```typescript
const tables = await grist.docApi.fetchTable("_grist_Tables");

const columns = await grist.docApi.fetchTable(
  "_grist_Tables_column"
);

const views = await grist.docApi.fetchTable(
  "_grist_Views"
);

const sections = await grist.docApi.fetchTable(
  "_grist_Views_section"
);

const fields = await grist.docApi.fetchTable(
  "_grist_Views_section_field"
);
```

A plugin can therefore reconstruct much of the document's metadata without
accessing the underlying SQLite database.

For example, the basic table/column relationship can be reconstructed using
the numeric row IDs in the metadata tables:

```typescript
const tables = await grist.docApi.fetchTable("_grist_Tables");
const columns = await grist.docApi.fetchTable(
  "_grist_Tables_column"
);

// columns.parentId refers to the numeric row ID of
// the corresponding _grist_Tables record.
```

---

# Particularly useful tables

For a plugin doing document introspection, the most useful tables are:

```text
_grist_DocInfo
_grist_Tables
_grist_Tables_column
_grist_Views
_grist_Views_section
_grist_Views_section_field
_grist_TabBar
_grist_Pages
_grist_Filters
_grist_Validations
_grist_Triggers
_grist_ACLRules
_grist_ACLResources
_grist_ACLPrincipals
_grist_ACLMemberships
```

In particular:

```text
_grist_Tables
       │
       └── _grist_Tables_column
                 │
                 └── column definitions

_grist_Views
       │
       └── _grist_Views_section
                 │
                 └── _grist_Views_section_field
                           │
                           └── displayed columns
```

These tables are enough to reconstruct a significant portion of Grist's
document model.

---

# Important caveat: internal API

Although these tables are accessible through `fetchTable()`, they should be
considered internal implementation details rather than a stable public
metadata API.

Their names, columns, semantics, and relationships may change between Grist
releases.

Plugins depending on these tables should therefore:

1. Detect the Grist/document schema version where appropriate.
2. Avoid assuming that every internal table exists.
3. Avoid assuming that every internal column exists.
4. Gracefully handle schema changes.
5. Prefer the documented plugin API when it provides the required information.

The current generated schema declares:

```typescript
export const SCHEMA_VERSION = 46;
```

---

# `_grist_*` versus `_gristsys_*`

The `_grist_*` tables should not be confused with `_gristsys_*` tables found
in the underlying `.grist` SQLite storage.

The two layers serve different purposes:

```text
Grist document engine
        │
        └── _grist_*
              ├── tables
              ├── columns
              ├── views
              ├── ACL
              ├── filters
              ├── triggers
              └── other document metadata

Grist document storage
        │
        └── _gristsys_*
              └── storage/server internals
```

The `_gristsys_*` schema is maintained by the document-storage layer and should
not be treated as part of the `_grist_*` document-engine schema.

---

# Complete table list

The current `_grist_*` schema contains:

```text
_grist_DocInfo
_grist_Tables
_grist_Tables_column
_grist_Imports
_grist_External_database
_grist_External_table
_grist_TableViews
_grist_TabItems
_grist_TabBar
_grist_Pages
_grist_Views
_grist_Views_section
_grist_Views_section_field
_grist_Validations
_grist_REPL_Hist
_grist_Attachments
_grist_Triggers
_grist_ACLRules
_grist_ACLResources
_grist_ACLPrincipals
_grist_ACLMemberships
_grist_Filters
_grist_Cells
_grist_Shares
```

---

# Source references

## Grist plugin API

GristDocAPI `fetchTable()`:

https://support.getgrist.com/code/interfaces/grist_plugin_api.GristDocAPI/#fetchtable

## Generated document schema

https://github.com/gristlabs/grist-core/blob/main/app/common/schema.ts

## Grist database documentation

https://github.com/gristlabs/grist-core/blob/main/documentation/database.md

## Grist Core repository

https://github.com/gristlabs/grist-core
