  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Slug": "🧹",
  >   "Nom": "Clean",
  >   "Fiche_de_poste": "",
  >   "Impose": "Non",
  >   "Specialiste_requis": false,
  >   "Decoupable": true,
  >   "Lieu_par_defaut": 1
  > }

  $ cat initial | ./grist_conv.exe task_type | jq | tee recoded
  {
    "id": 1,
    "Slug": "🧹",
    "Nom": "Clean",
    "Fiche_de_poste": "",
    "Impose": "Non",
    "Specialiste_requis": false,
    "Decoupable": true,
    "Lieu_par_defaut": 1
  }

  $ diff initial recoded

  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Slug": "🌍",
  >   "Nom": "Site",
  >   "Description": ""
  > }

  $ cat initial | ./grist_conv.exe lieu | jq | tee recoded
  {
    "id": 1,
    "Slug": "🌍",
    "Nom": "Site",
    "Description": ""
  }

  $ diff initial recoded


  $ cat >initial <<'EOF'
  > {
  >   "id": 1,
  >   "Pseudo": "f",
  >   "Nom": "",
  >   "Prenom": "Pouet2",
  >   "Nb_heures": 4,
  >   "Langues": [
  >     "L",
  >     "Français",
  >     "Anglais"
  >   ],
  >   "Telephone": "",
  >   "Email": "",
  >   "Specialites": null,
  >   "Taches_interdites": null,
  >   "Lieux_interdits": null,
  >   "Amis": [
  >     "L",
  >     1
  >   ],
  >   "Ennemis": [
  >     "L",
  >     1
  >   ],
  >   "Date_d_arrivee": null,
  >   "Date_de_depart": null,
  >   "Indisponibilites_quotidiennes": null,
  >   "Horaires_preferes": null,
  >   "Horaires_contraints": null,
  >   "Indisponibilites_ponctuelles": null
  > }
  > EOF


  $ cat initial | ./grist_conv.exe benevole | jq >recoded

  $ diff initial recoded
