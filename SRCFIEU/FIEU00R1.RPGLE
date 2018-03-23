000200051109     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000201061106     C*---------------------------------------------------------------*
000202061106     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
000203061106     C*     per segnalare il Depot/Cliente mancante sulla tabella     *
000204061106     C*     DPPDC00F                                                  *
000205061106     C*---------------------------------------------------------------*
000207061106     c     *entry        plist
000208070702     c                   parm                    Indirizzi       253
000209070702     c                   parm                    Oggetto          44
000210070702     c                   parm                    Messaggio      5000
000212061106      *
000213061106     C* Inizializzo variabili
000214061106     C                   movel     *blanks       wrkEml          253
000215061106     C                   movel     *blanks       wrkMsg         5000
000216061106     C                   movel     *blanks       wrkOgg           44
000217061106      *
000218061106     C* Valorizzo i campi della e-m@ail - indirizzo
000219070702     C                   eval      wrkEml= Indirizzi
000220070702     C                   eval      wrkOgg= Oggetto
000221070702     C                   eval      wrkMsg= Messaggio
000253061106     C*
000254061106     C                   call(e)   'TIS701C'
000255061106     C                   parm                    wrkEml
000256061106     C                   parm                    wrkOgg
000257061106     C                   parm                    wrkMsg
007800051109
007900990504     c                   seton                                        lr
