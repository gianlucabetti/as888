000200051109     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000201061106     C*---------------------------------------------------------------*
000202061106     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
000203061106     C*     per segnalare il Depot/Cliente mancante sulla tabella     *
000204061106     C*     DPPDC00F                                                  *
000205061106     C*---------------------------------------------------------------*
000207061106     c     *entry        plist
000208061106     c                   parm                    cod_dep           4
000209061106     c                   parm                    cod_cli           7
000210061108     c                   parm                    nr_parcel        14
000211061108     c                   parm                    nr_Bolla         19
000212061106      *
000213061106     C* Inizializzo variabili
000214061106     C                   movel     *blanks       wrkEml          253
000215061106     C                   movel     *blanks       wrkMsg         5000
000216061106     C                   movel     *blanks       wrkOgg           44
000217061106      *
000218061106     C* Valorizzo i campi della e-m@ail - indirizzo
000219061106     C                   eval      wrkEml= 'CED@bartolini.it'
000220061115
000221061115      **** significa che il depot esiste e manca solo il codice cliente
000222061115     c                   if        cod_cli = *all'9'
000223061115
000224061115     C                   eval      wrkOgg='CODICE CLIENTE DPD non impostato +
000225061115     C                             sulla tabella dei Depot x eseguire il Reso'
000226061115
000228061115     C                   eval      wrkMsg = 'Attenzione: Nel file DPPDC00F,'   +
000229061115     C                             ' (File dei Depot DPD da cappario DPD),'    +
000230061115     C                             ' MANCA  << il cod.Cliente >>  per ' +
000231061115     C                             'poter eseguire il reso dal FNLG20R. '      +
000232061115     C                             'Il codice Depot DPD è: ' + Cod_dep         +
000233061115     C                             ' Il cod.Cliente è: 0000000 '               +
000234061115     C                             ' Il Parcel è: '          + nr_parcel       +
000235061115     C                             ' della Bolla: '          + nr_Bolla
000236061115
000237061115     c                   else
000238061115      **** altrimenti manca il Depot sul file DPPDC
000239061106
000240061106      * Oggetto della Mail da inviare
000241061115     C                   eval      wrkOgg='DEPOT DPD MANCANTE sulla tabella    +
000242061115     C                             x fare il reso'
000243061106
000244061106     C                   eval      wrkMsg = 'Attenzione: Nel file DPPDC00F,'   +
000245061108     C                             ' (File dei Depot DPD da cappario DPD),'    +
000246061115     C                             ' MANCA il cod.DEPOT per ' +
000247061106     C                             'poter eseguire il reso dal FNLG20R. '      +
000248061108     C                             'Il codice Depot DPD è: ' + Cod_dep         +
000250061108     C                             ' Il Parcel è: '          + nr_parcel       +
000251061108     C                             ' della Bolla: '          + nr_Bolla
000252061115     c                   end
000253061106     C*
000254061106     C                   call(e)   'TIS701C'
000255061106     C                   parm                    wrkEml
000256061106     C                   parm                    wrkOgg
000257061106     C                   parm                    wrkMsg
007800051109
007900990504     c                   seton                                        lr
