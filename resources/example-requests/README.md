# Requests and Responses

I don't have a swagger / open api file ... yet.

Basically, there are 3 requests:

- connection
- build
- eval

See the relevant har files in this folder.

## Connection

Request:
```
GET http://localhost:33333/api/v1/connection
```

Response (content omitted for brevity):
```
{
  "result": {
    "connection-id": "avallone-local",
    "metadata": {
      "db/references": {
        "table": {
          "company": {
            "in": {
              "public": {
                "refers-to": {
                  "tenant": {
                    "in": {
                      "public": {
                        "via": {
                          "tenantId": [
                            "public",
                            "company",
                            "tenantId",
                            "=",
                            "public",
                            "tenant",
                            "id"
                          ]
                        }
                      }
                    }
                  }
                },
                "referred-by": {
                }
              }
            },
            "refers-to": {
              "tenant": {
                "via": {
                  "tenantId": [
                    [
                      "public",
                      "company",
                      "tenantId",
                      "=",
                      "public",
                      "tenant",
                      "id"
                    ]
                  ]
                }
              }
            },
            "referred-by": {
            }
          }
        },
        "schema": {
        }
      }
    }
  }
}
```

## Build

Request:

```
POST http://localhost:33333/api/v1/build
{"expression":"tenant | company | l: 1"}
```


Response:

```
{
    "connection-id": "avallone-local",
    "query": "SELECT company_1.* FROM \"tenant\" AS tenant_0 JOIN \"company\" AS company_1 ON (company_1.\"tenantId\" = tenant_0.\"id\") WHERE true AND true LIMIT 1",
    "params": [],
    "hints": {},
    "context": [
        {
            "schema": "public",
            "alias": "tenant_0",
            "table": "tenant"
        },
        {
            "schema": "public",
            "alias": "company_1",
            "table": "company"
        }
    ]
}
```

## Eval

Request:

```
POST http://localhost:33333/api/v1/eval
{"expression":"tenant | company | l: 1"}
```

Response:

```
{
  "connection-id": "avallone-local",
  "query": "SELECT company_1.* FROM \"tenant\" AS tenant_0 JOIN \"company\" AS company_1 ON (company_1.\"tenantId\" = tenant_0.\"id\") WHERE true AND true LIMIT 1",
  "params": [],
  "result": [
    [
      "id",
      "name",
      "tenantid"
    ],
    [
      "8249d682-e1ee-4ff6-b205-db58bd7ed525",
      "company",
      "34ec68cf-1711-4486-8541-1cb9f9bee56d"
    ]
  ]
}```
