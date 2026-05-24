  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "slug": "🧹",
  >   "nom": "Clean",
  >   "fiche_de_poste": "",
  >   "impose": "Non",
  >   "specialiste_requis": false,
  >   "decoupable": true,
  >   "free": false
  > }

  $ cat initial | ./grist_conv.exe task_type | jq | tee recoded
  {
    "id": 1,
    "slug": "🧹",
    "nom": "Clean",
    "fiche_de_poste": "",
    "impose": "Non",
    "specialiste_requis": false,
    "decoupable": true,
    "free": false
  }

  $ diff initial recoded

  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "slug": "🌍",
  >   "nom": "Site",
  >   "description": ""
  > }

  $ cat initial | ./grist_conv.exe lieu | jq | tee recoded
  {
    "id": 1,
    "slug": "🌍",
    "nom": "Site",
    "description": ""
  }

  $ diff initial recoded


  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "pseudo": "f",
  >   "nom": "",
  >   "prenom": "Pouet2",
  >   "nb_heures": 4,
  >   "langues": [
  >     "L",
  >     "Français",
  >     "Anglais"
  >   ],
  >   "telephone": "",
  >   "email": "",
  >   "specialites": null,
  >   "taches_interdites": null,
  >   "amis": [
  >     "L",
  >     1
  >   ],
  >   "ennemis": null,
  >   "date_d_arrivee": null,
  >   "date_de_depart": null,
  >   "indisponibilites_quotidiennes": [
  >     "L",
  >     8,
  >     9,
  >     11
  >   ],
  >   "horaires_preferes": null,
  >   "horaires_contraints": null,
  >   "indisponibilites_ponctuelles": null
  > }
  > EOF


  $ cat initial | ./grist_conv.exe benevole | jq >recoded

  $ diff initial recoded


  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "manualSort": 1,
  >   "nom": "Clean 1",
  >   "type_": 1,
  >   "lieu": 1,
  >   "recurrence": "Ponctuelle",
  >   "jours": null,
  >   "date_et_heure_de_debut": 1775752200,
  >   "benevoles": 2,
  >   "duree_heures": 2,
  >   "date_et_heure_de_fin": 1775759400,
  >   "fin_de_recurrence": 1775759400,
  >   "modCount": 6,
  >   "benevoles_assignes": [
  >     "L",
  >     1
  >   ],
  >   "gristHelper_Display2": "🧹 Clean",
  >   "gristHelper_Display": "🌍 Site",
  >   "gristHelper_Display4": [
  >     "L",
  >     "jjj"
  >   ],
  >   "display": "🧹 Clean"
  > }
  > EOF

  $ cat initial | ./grist_conv.exe quete | jq >recoded
