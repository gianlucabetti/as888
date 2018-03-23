000100990504     H* ACTGRP=*CALLER
000200051109     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000300990504     d dataiso         s               d   datfmt(*iso)
000400051110     d dataiso2        s               d   datfmt(*iso)
000401130730     d dmraceg       e ds
000402130730     d kpjba         e ds
000403130730     D TIBS02DS      E DS
000500030225     d Wdata8          DS
000600030225     d  dadata                 1      8  0
000700030225     d  adata                  9     16  0
000800030225     d  GioLav                17     21  0
000900030225      *
000901130730     c     *entry        plist
000902130730     c                   parm                    kpjba
000903130730      *
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
003301130730     C                   movel     *blanks       wrkEmcc         253
003400051109     C                   movel     *blanks       wrkMsg         5000
003500051109     C                   movel     *blanks       wrkOgg           44
003501130730     C                   movel     *blanks       wrkesit           1
003600130730     C* Valorizzo i campi della e-m@ail tramite tabella MRA
003602130730     c                   clear                   dmraceg
003603130730     c                   clear                   tibs02ds
003604130730     c                   eval      t02mod = 'C'
003605130730     c                   eval      t02sif = knsif
003606130730     c                   eval      t02cod = 'MRA'
003607130730     c                   eval      t02ke1 = 'FICN34R1'
003608130730     c                   call      'TIBS02R'
003609130730     c                   parm                    kpjba
003610130730     c                   parm                    tibs02ds
003611130730     c                   if        t02err = *blanks
003612130730     c                   eval      dmraceg = t02uni
003615130730     c                   endif
003616130730     c
003617130730     C                   eval      wrkEml= §mraem
003700130730     C                   eval      wrkEmcc= §mracc
004900051129
005000051109     C                   eval      wrkOgg='Lancio fine mese C.E GIORNALIERO'
005100051124     C                   eval      wrkMsg='Attenzione l''ultimo giorno del ' +
005200051124     C                             'mese cade di sabato o domenica.'+':/N'+
005300130730     C                             'Pertanto la mattina del 3° giorno l' +
005400130730     C                             'avorativo dopo il fine mese si dovrà'+':/N'+
005500051124     C                             'richiedere l''elaborazione del C.E.giorna' +
005600130730     C                             'liero utilizzando la funzione '+':/N'+
005700130730     C                             '"Stampa CE GIORNALIERO per elaborazione ' +
005701130801     C                             'fine mese" presente nel menù '+':/N'+
005800130730     C                             'CONTROLLO DI GESTIONE PER RESP.PROJ"'+':/N'+
006600130730     C                             'Prima del lancio è necessario acquisire '+
006800130730     C                             'i dati dal file in quanto verranno '+
006900130801     C                             'sovrascritti    '+':/N'
007100051109     C*
007200130730     C                   call(e)   'TIS701C1'
007300051109     C                   parm                    wrkEml
007301130730     C                   parm                    wrkEmcc
007400051109     C                   parm                    wrkOgg
007500051109     C                   parm                    wrkMsg
007501130730     C                   parm                    wrkesit
007600060926
007700051109     c                   endif
007800051109
007900990504     c                   seton                                        lr
