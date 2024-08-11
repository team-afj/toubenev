from typing import List, Dict

import json
from datetime import date, time, datetime, timedelta

from data_model import Bénévole, Lieu, Type_de_quête, Quête


def to_json(result: Dict[Quête, List[Bénévole]]):
    json_arr = []
    for q, participants in result.items():
        quête = {}
        quête["name"] = q.nom
        quête["quest_id"] = q.id
        quête["volunteers_id"] = list(map(lambda b: b.id, participants))
        quête["start"] = q.début.isoformat(sep="T")
        quête["end"] = q.fin.isoformat(sep="T")
        quête["locked"] = False
        json_arr.append(quête)
    return json.dumps(json_arr)


def write_json(result: Dict[Quête, List[Bénévole]], file="result.json"):
    acc = {}
    splits = []
    i = 0
    for q, l in result.items():
        i += 1
        acc[q] = l
        if i >= 50:
            splits.append(acc)
            acc = {}
            i = 0
    if i >= 0:
        splits.append(acc)
    i = 0
    for split in splits:
        i += 1
        name = f"{file}.{i}.json"
        result = to_json(split)
        with open(name, "w") as text_file:
            text_file.write(result)


# [
#    {
#       "name":"Nom de la quête",
#       "quest_id":"04dabbe6d82b45d3a3bd1d357aad3098",
#       "volunteers_id":[
#          "e8dc719d-49a2-4605-b9ca-c9ec1adf4589",
#          "2ec5761c-473a-4a8a-ab1e-de5622ae0871"
#       ],
#       "start":"2024-07-03T14:20:00.000000Z",
#       "end":"2024-07-03T18:20:00.000000Z",
#       "locked":true
#    }
# ]
