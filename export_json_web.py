from typing import List, Dict

import json
from datetime import date, time, datetime, timedelta

from data_model import Bénévole, Lieu, Type_de_quête, Quête


def types():
    return [{"id": tdq.id, "name": tdq.nom} for tdq in Type_de_quête.tous.values()]


def places():
    return [{"id": p.id, "name": p.nom} for p in Lieu.tous.values()]


def volunteers():
    return [{"id": b.id, "pseudo": b.surnom} for b in Bénévole.tous.values()]


def quests(result: Dict[Quête, List[Bénévole]]):
    arr = []
    for q, participants in result.items():
        quête = {}
        quête["id"] = q.id
        quête["name"] = q.nom
        quête["types"] = list(map(lambda tdq: tdq.id, q.types))
        quête["place"] = q.lieu.id
        quête["volunteers"] = list(map(lambda b: b.id, participants))
        quête["start"] = q.début.isoformat(sep="T")
        quête["end"] = q.fin.isoformat(sep="T")
        arr.append(quête)
    return arr


def write_json(assignations: Dict[Quête, List[Bénévole]], file="result.json"):
    name = f"{file}.json"
    result = {}
    result["quest_types"] = types()
    result["places"] = places()
    result["volunteers"] = volunteers()
    result["quests"] = quests(assignations)
    with open(name, "w") as text_file:
        text_file.write(json.dumps(result))


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
