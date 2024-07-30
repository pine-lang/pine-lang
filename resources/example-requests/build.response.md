# Example

Pine Expression:

```pine
tenant | s: title | public.company .tenantId | where: name like 'Acme Inc%' | 10 | atta
```

Response to `api/v1/build`:

```json
{
    "connection-id": "localhost",
    "version": "0.5.4",
    "query": "\nSELECT \"t_0\".\"title\", a_2.* FROM \"tenant\" AS \"t_0\" JOIN \"public\".\"company\" AS \"c_1\" ON \"c_1\".\"tenantId\" = \"t_0\".\"id\" JOIN \"atta\" AS \"a_2\" ON \"\" = \"\" WHERE \"c_1\".\"name\" like 'Acme Inc%' LIMIT 10;\n",
    "state": {
        "hints": {
            "table": [
                {
                    "schema": "public",
                    "table": "attachment",
                    "column": "attachment_id",
                    "child": true,
                    "pine": "public.attachment .attachment_id"
                }
            ]
        },
        "table-count": 3,
        "where": [
            [
                "c_1",
                "name",
                "like",
                "Acme Inc%"
            ]
        ],
        "selected-tables": [
            {
                "schema": null,
                "table": "tenant",
                "alias": "t_0"
            },
            {
                "schema": "public",
                "table": "company",
                "alias": "c_1"
            }
        ],
        "limit": 10,
        "pending-count": 0,
        "columns": [
            {
                "column": "title",
                "alias": "t_0"
            }
        ],
        "operation": {
            "type": "table",
            "value": {
                "table": "atta"
            }
        },
        "join-map": {
            "t_0": {
                "c_1": [
                    "c_1",
                    "tenantId",
                    "=",
                    "t_0",
                    "id"
                ]
            },
            "c_1": {
                "a_2": null
            }
        },
        "tables": [
            {
                "schema": null,
                "table": "tenant",
                "alias": "t_0"
            },
            {
                "schema": "public",
                "table": "company",
                "alias": "c_1"
            },
            {
                "schema": null,
                "table": "atta",
                "alias": "a_2"
            }
        ],
        "context": "a_2",
        "connection-id": "default",
        "aliases": {
            "t_0": {
                "table": "tenant",
                "schema": null
            },
            "c_1": {
                "table": "company",
                "schema": "public"
            },
            "a_2": {
                "table": "atta",
                "schema": null
            }
        }
    },
    "deprecation-notice": "Properties will be removed in the next major version: `hints`, `context`. Use `state` (`hints`, `selected-tables`) instead.",
    "hints": {
        "table": [
            {
                "schema": "public",
                "table": "attachment",
                "column": "attachment_id",
                "child": true,
                "pine": "public.attachment .attachment_id"
            }
        ]
    },
    "context": [
        {
            "schema": null,
            "table": "tenant",
            "alias": "t_0"
        },
        {
            "schema": "public",
            "table": "company",
            "alias": "c_1"
        }
    ]
}
```
