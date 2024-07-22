import json
from datetime import date, time, datetime, timedelta

from data_model import Bénévole, Spectacle, Lieu, Type_de_quête, Quête


def load_spectacles(obj):
    spectacles = obj["shows"]
    for s in spectacles["pages"]:
        id = s["id"]
        props = s["properties"]
        Spectacle(id, props["Nom"]["title"][0]["plain_text"])


def load_lieux(obj):
    places = obj["places"]
    for p in places["pages"]:
        id = p["id"]
        props = p["properties"]
        Lieu(id, props["Name"]["title"][0]["plain_text"])


def load_types_de_quête(obj):
    quest_types = obj["questsTypes"]
    for p in quest_types["pages"]:
        id = p["id"]
        name = p["properties"]["Name"]["title"][0]["plain_text"]
        sécable = p["properties"]["D\u00e9coupable ?"]["checkbox"]
        Type_de_quête(id, name, sécable)


def update_pref_horaire(heure_début, data, prefs, indisponibilités):
    heure_fin = heure_début + 1
    if heure_début == 23:
        heure_fin = 0
    début = f"{heure_début:0=2d}"
    if heure_début == 9:
        début = f"{heure_début}"
    select = data[f"{début}h - {heure_fin:0=2d}h"]["select"]
    pref = "no pref"
    if select:
        pref = select["id"]
    val = 0
    if pref == "E>t^":  # Indisponible
        indisponibilités.append(time(hour=heure_début))
        return
    if pref == "P;>X":  # Sous la contrainte
        val = -5
    if pref == "Sa=z":  # Horaire de prédilection
        val = 2
    prefs[time(hour=heure_début)] = val


def load_bénévoles(obj):
    volunteers = obj["volunteers"]
    for p in volunteers["pages"]:
        props = p["properties"]
        id = p["id"]
        sérénité = props["Team S\u00e9r\u00e9nit\u00e9"]["checkbox"]
        indisponibilités = []
        pref_horaires = {}
        update_pref_horaire(0, props, pref_horaires, indisponibilités)
        for i in range(9, 23 + 1):
            update_pref_horaire(i, props, pref_horaires, indisponibilités)
        # print(props["Name"]["title"][0]["plain_text"], pref_horaires)
        b = Bénévole(
            id,
            props["Pseudo"]["title"][0]["plain_text"],
            props["Pr\u00e9nom"]["rich_text"][0]["plain_text"],
            props["Nom"]["rich_text"][0]["plain_text"],
            int(props["heures th\u00e9oriques par jour"]["number"]),
            indisponibilités,
            pref_horaires,
            sérénité,
        )


def parse_horaires(horaire: str):
    horaire_groups = re.match(r"(.*) \(.*\) → (.*) \(.*\)", horaire)
    if horaire_groups:
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        fin = datetime.strptime(horaire_groups.group(2), "%d/%m/%Y %H:%M")
        return début, fin
    else:
        horaire_groups = re.match(r"(.*) \(.*\) → (.*):(.*)", horaire)
        début = datetime.strptime(horaire_groups.group(1), "%d/%m/%Y %H:%M")
        heure_fin = time(int(horaire_groups.group(2)), int(horaire_groups.group(3)))
        fin = datetime.combine(début, heure_fin)
        return début, fin


def load_quêtes(obj):
    quests = obj["quests"]
    for p in quests["pages"]:
        props = p["properties"]
        id = p["id"]
        name = props["Name"]["title"][0]["plain_text"]
        needed = props["Needed"]["number"]
        places = props["Place"]["relation"]
        place = None
        if len(places) > 0:
            place = Lieu.tous[places[0]["id"]]
        types_de_quête = list(
            map(
                lambda tdq: Type_de_quête.tous[tdq["id"]],
                props["Type de Quete"]["relation"],
            )
        )
        début = datetime.fromisoformat(props["Horaire"]["date"]["start"])
        fin = datetime.fromisoformat(props["Horaire"]["date"]["end"])
        bénévoles = list(
            map(
                lambda b: Bénévole.tous[b["id"]],
                props["B\u00e9n\u00e9voles v\u00e9rouill\u00e9s"]["relation"],
            )
        )
        spectacle = None
        if len(props["Spectacle"]["relation"]) > 0:
            spectacle = Spectacle.tous[props["Spectacle"]["relation"][0]["id"]]

        if all(t.sécable for t in types_de_quête):
            fin_acc = début
            i = 0
            while fin_acc < fin:
                début_acc = fin_acc
                # fin_acc = min(fin_acc + timedelta(minutes=15), fin)
                fin_acc = min(fin_acc + timedelta(minutes=120), fin)
                i = i + 1
                Quête(
                    id,
                    f"{name} #{i}",
                    types_de_quête,
                    place,
                    spectacle,
                    needed,
                    début_acc,
                    fin_acc,
                    bénévoles,
                )
        else:
            Quête(
                id,
                name,
                types_de_quête,
                place,
                spectacle,
                needed,
                début,
                fin,
                bénévoles,
            )


def from_file(src):
    with open(src, "r") as file:
        obj = json.load(file)
        load_spectacles(obj)
        load_lieux(obj)
        load_types_de_quête(obj)
        load_bénévoles(obj)
        load_quêtes(obj)
