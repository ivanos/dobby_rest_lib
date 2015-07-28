# dobby_rest
RESTish interface to dobby, the graph store

This repository, dobby_rest_lib, is the implementation of a RESTish
interface to Dobby which
can be used as a dependency to build nodes providing a REST interface
to Dobby.
https://github.com/ivanos/dobby_rest_node.git runs Dobby REST as a
standalone service.

###Requirements
1. [dobby](https://github.com/ivanos/dobby_node).
2. Erlang R17

###Building
make compile

Note that some request errors are reported to the console. Check the logs if dobby_rest_lib returns status 500 to the REST client.

###Testing
You can use the Google Chrome extension [Postman](https://chrome.google.com/webstore/detail/postman-rest-client/fdmmgilgnpjigdojojpjoooidkmcomcm?hl=en)
to test the REST calls. Import the dobby_rest.json collection for some examples.

###HTTP response codes

Response code | Description
--- | ---
200   | Success
201   | Successfully created resource
204   | Successfully deleted resource
404   | Not Found
422   | POST request malformed/incomplete
500   | Server Error

###Identifiers

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
Get identifier details  | /identifier/identifier_name   | `GET`     | n/a           | Example 1 
Create identifier       | /identifier/identifier_name   | `POST`    | Example 2     | true/false
Delete identifier       | /identifier/identifier_name   | `DELETE`  | n/a           | true/false

- In the response object, the identifier value is URL encoded.
- Deleting a identifier will remove associated links.

####Example 1 - identifier JSON response object:

```
{
    "identifier":"name2",
    "metadata":{
       "key":{
         "value":1,
         "timestamp":"2015-05-01T23:22:14Z",
         "publisher_id":"dobby_rest"
       },
       "key2":{
         "value":"2",
         "timestamp":"2015-05-01T23:22:14Z",
         "publisher_id":"dobby_rest"
       }
    }
}
```

####Example 2 - create identifier JSON request body

```
{"metadata":{"key":1,"key2":"2"}}
```

###Links

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
create link     | /link/id1/id2        | `POST`    | Example 3 | true/false
get link        | /link/id1/id2        | `GET`     | n/a       | Example 4
delete link     | /link/id1/id2        | `DELETE`  | n/a       | true/false

- Missing vertices will be created when adding a link.
- Links are not directed so link names are symmetric. That is idA/idB is the same as idB/idA.

####Example 3 - create link request body

```
{
  "metadata":{
    "ip":"10.151.1.68"
  }
}
```

####Example 4 - link JSON response object

```
{
  "link":"id1/id2",
  "metadata":{
    "ip":{
      "value":"10.151.1.68",
      "timestamp":"2015-06-04T02:51:32Z",
      "publisher_id":"dobby_rest"
  }
}
```

###Identifier Metadata

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
get identifier metadata property    | /identifier/identifier_name/metadata/ip             | `GET`     | n/a       | "1"
add or update identifier metadata             | /identifier/identifier_name/metadata/ip    | `POST`    | "2.2.2.2" | true/false
remove identifier metadata          | /identifier/identifier_name/metadata/ip    | `DELETE`  | n/a       | true/false

###Link Metadata

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
get link metadata property          | /link/id1/id2/metadata/creation_datetime     | `GET`     | n/a       | "2014-07-16T19:20:30+01:00"
add link metadata                   | /link/id1/id2/metadata/key          | `POST`    | "value"   | true/false
remove link metadata                | /link/id1/id2/metadata/key          | `DELETE`  | n/a       | true/false

###Search

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
search on identifier  | /identifier/vname1%2F2/search    |   `POST`  |   Example 6 | Example 8

###Example 6 - search request JSON body
```
{
  "max_depth":1,
  "traversal":"depth",
  "max_size":100,
  "match_metadata":{
      "type":["IPV4", "IPV6"]
  },
  "match_links":{
    "type":"IP-MAC"
  }
  "results_filter":["capabilities"],
  "match_terminal":{
    "type":"device"
  }
}
```
Parameter | Description
--- | ---
max_depth       | maximum search depth from starting identifier (e.g., vname1)
traversal | graph traversal algorithm (`depth` for depth first or `breadth` for breadth first)
max_size        | maximum number of identifiers in the result set (not implemented)
match_metadata  | only follow links from identifiers with metadata matching these key/value pairs. The identifier's metadata must match all of the key/value pairs. If not given or `all` (instead of a list) then all links are followed.
match_links     | only follow links with metadata matching these key/value pairs. The link's metadata must match all of the key/value pairs. If not given or `all` (instead of a list) then all links are followed.
match_terminal  | stop the search when the identifer has metadata matching these key/value pairs. The identifier's metadata mush match all of the key/value pairs. The identifier and link is included in the result. If not given or `none` (instead of a list) then the search does not terminate by with a metadata match.
results_filter  | list of metadata keys to include in the metadata in the results. If not given or `all` (instead of a list), then all of the metadata is included.
match_path      | Describes a desired path in the graph to match against. All identifiers in matching paths are included in the results. The desired path is a list of elements indicating metadata to match against of the link or of the identifier in the path. The desired path may be a mix of identifiers and links. `match_links` and `match_metadata` may be used in conjunction with `match_path` in which case the `match_path` is checked after the other matches. See Example 8.

- For match_metadata, match_links, and match_terminal the value of the key/value match pair may be a single element or a list. If the value is a single element then a match occurs when the metadata value in dobby matches the single element. If the value in the key/value match pair is a list then a match occurs when the metadata avlue is dobby matches one of the elements of the list.
- The Result is a list of identifiers and links.
- In the response object, the identifier value and the identifiers in the link value are URL encoded.

###Example 7 - match_path example
```
{
  "max_depth":10,
  "traversal":"depth",
  "match_path":[
    [
     {"element":"identifier","metadata":{"type":"of_switch"}},
     {"element":"link","metadata":{"type":"port_of"}},
     {"element":"identifier","metadata":{"type":"of_port"}},
     {"element":"link","metadata":{"type":"connected_to"}},
     {"element":"identifier","metadata":{"type":"of_port"}},
     {"element":"link","metadata":{"type":"port_of"}},
     {"element":"identifier","metadata":{"type":"of_switch"}}
    ],
    [
     {"element":"identifier","metadata":{"type":"of_switch"}},
     {"element":"identifier","metadata":{"type":"of_port"}},
     {"element":"link","metadata":{"type":"connected_to"}},
     {"element":"identifier","metadata":{"type":"endpoint"}}
    ]
  ]
}
```
Looks for two different paths. The first path is a connection between
two switches (of_switch, linked via a port_of to an of_port, the of_port linked via a connected_to to another of_port, linked via a port_of to an of_switch), and between a switch and an endpoint (of_switch linked by an unspecified link type to an of_port linked via a connected_to to an endpoint).

###Example 8 - search response JSON
```
[
    {
        "identifiers":[
            {
              "identifier":"vname1%2F2",
              "metadata":{
                "type":{
                  "value":"resource",
                   "timestamp":"2015-05-01T23:22:14Z",
                   "publisher_id":"dobby_rest"
                },
                "ip":{
                  "value":"9.9.9.9"
                   "timestamp":"2015-05-01T23:22:14Z",
                   "publisher_id":"dobby_rest"
                }
              }
            },
            {
              "identifier":"vname2",
              "metadata":{
                "type":{
                  "value":"resource",
                   "timestamp":"2015-05-01T23:22:14Z",
                   "publisher_id":"dobby_rest"
                },
                "ip":{
                  "value":"8.8.8.8"
                   "timestamp":"2015-05-01T23:22:14Z",
                   "publisher_id":"dobby_rest"
                }
              }
            }
        ],
        "links":[
            {
              "link":"vname1%2F2/vname2"
              "metadata":{
                "type":{
                  "value":"connection"
                   "timestamp":"2015-05-01T23:22:14Z",
                   "publisher_id":"dobby_rest"
                }
              }
            }
        ]
]
```
