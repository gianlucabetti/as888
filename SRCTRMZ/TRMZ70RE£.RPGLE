000100990504     H* ACTGRP=*CALLER
000200051109     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000300990504     d dataiso         s               d   datfmt(*iso)
000400051110     d dataiso2        s               d   datfmt(*iso)
000500030225     d Wdata8          DS
000600030225     d  dadata                 1      8  0
000700030225     d  adata                  9     16  0
000800030225     d  GioLav                17     21  0
000900030225      *
001000051109     c                   move      *date         dataiso
001100051109      *
001200051109     c     dataiso       adddur    1:*d          dataiso2
001300051109     c                   do        *hival
001400051109     c                   move      dataiso       dadata
001500051109     c                   move      dataiso2      adata
001600051109     c                   CALL      'XSRLAV8'
001700051109     c                   PARM                    Wdata8
001800051110      * se maggiore di 1 attivo i controlli per il confronto delle date
001900051110     c                   if        giolav <> 0
002000051109      *__________ verifico se il mese delle date in esame è uguale _______
002100051109     c                   extrct    dataiso:*m    mese1             2 0
002200051109     c                   extrct    dataiso2:*m   mese2             2 0
002300051109     c                   leave
002400051109     c                   else
002500051109     c     dataiso2      adddur    1:*d          dataiso2
002600051109     c                   iter
002700051109     c                   endif
002800051109     c                   enddo
002900051109
003000051110      *se il mese è differente sono nel caso interessato mando la mail ___
003100051109     c                   if        mese1 <> mese2
003200051109     C* Inizializzo variabili
003300051109     C                   movel     *blanks       wrkEml          253
003400051109     C                   movel     *blanks       wrkMsg         5000
003500051109     C                   movel     *blanks       wrkOgg           44
003600051109     C* Valorizzo i campi della e-m@ail
003700060927     C                   eval      wrkEml= 'Michele.Malipiero@bartolini.it'
003800051129      *le specifiche sotto andranno disasteriscate quando sarà
003900051129      * possibile gestire più di un indirizzo tramite l'utility di Fabrizio
004000060927      * nella quale per gestire più indirizzi vanno tolte le parentesi attuali
004100060927      * L'invio a Ced@bartolini.it non è necessario è già di Default
004200051129      *
004300060927     C*                  eval      wrkEml =
004400060927     C*                            '(''Michele.Malipiero@bartolini.it'')'
004500060927     C*                            + ' ' +
004600060927     C*                            '(''Wanda.Balestri@bartolini.it'')'
004700060927     C*                            + ' ' +
004800060927     C*                            '(''Ced@bartolini.it'')'
004900051129
005000051109     C                   eval      wrkOgg='Lancio fine mese C.E GIORNALIERO'
005100051124     C                   eval      wrkMsg='Attenzione l''ultimo giorno del ' +
005200051124     C                             'mese cade di sabato o domenica.'+':/N'+
005300051124     C                             'Pertanto la mattina del 3° giorno ' +
005400051124     C                             'lavorativo dopo il fine mese si deve'+':/N'+
005500051124     C                             'richiedere l''elaborazione del C.E.giorna' +
005600051124     C                             'liero utilizzando la funzione "FN33"'+':/N'+
005700051124     C                             '"lancio stampa CE GIORNALIERO" contenuto ' +
005800051124     C                             'nel MENU- CONTABILITA''/COOPERATIVE '+
005900051124     C                             'USO CED/CONTO ECONOMICO GIORNALIERO '+':/N'+
006000051124     C                             'IMPOSTANDO QUESTI PARAMETRI: es: '+':/N'+
006100051124     C                             'periodo dal xx/xx/xxXX al xx/xx/xxXX (' +
006200051124     C                             'considerando tutto il mese in esame)'+':/N'+
006300051124     C                             'Distretti 1 2 3 4 5 6               '+':/N'+
006400051124     C                             'Stampe in filiale  "S"              '+':/N'+
006500051124     C                             'Rigenerazione riepiloghi CDG.  "S"  '+':/N'+
006600051124     C                             'Prima del lancio è necessario accord'  +
006700051124     C                             'arsi con il CDG affinchè acquisisca '+':/N'+
006800051124     C                             'i dati dal file rigenerato e comunichi ai' +
006900051124     C                             ' vari responsabili la disponibilità'+':/N'+
007000051124     C                             'dell''elaborazione di fine mese.'
007100051109     C*
007200051109     C                   call(e)   'TIS701C'
007300051109     C                   parm                    wrkEml
007400051109     C                   parm                    wrkOgg
007500051109     C                   parm                    wrkMsg
007600060926
007700051109     c                   endif
007800051109
007900990504     c                   seton                                        lr
