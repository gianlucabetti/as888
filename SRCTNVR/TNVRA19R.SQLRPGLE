000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090424     H* TNVRA18R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090427     Ffitgt01l  uf   E           K DISK    infDS(DSTGT)
000900090429     Ffifgt01l  uF   E           k DISK
001000090109      *
001100090424     FwxFGT00S  o  a E             DISK
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300081211      * -------------------------------------------------------------
001400090427     dficn02ds       e ds
001500090421     d dapdflr       e ds
001600081110     D esito           s             10i 0
001700090427      *
001800090427     d dsTGT           ds
001900090427     d  tgt_rec              397    400b 0
002000090109      *
002100090427     C*=================================================================
002200090427      **  Deve allineare le Date di Convalida sul FITGT00F
002300090427     C*    ed alcuni dati anagrafici presi da FIAPD
002400090423     C*=================================================================
002500090430     c                   except    testa
002600090427     C                   SETON                                        LR
002700090427     c     *loval        setll     fitgt01l
002800090427     c                   read      fitgt01l
002900090109      * >>>>>>>
003000081209      * resto nel ciclo finchè....
003100090427     C                   DOw       not %EOF(fitgt01l)
003200090427      * >>>>>>>
003300081110      *
003400090427      *  se sono qui ho letto un record correttamente
003500090427     c   Of              except    testa
003600090427     c                   except    detta
003700090427      *
003800090427     c                   if        Aggiorna <> *blank
003900090423      *------------------
004000090427      *  Controlla se l'AUT per il progressivo nuovo ha tutte le tariffe Convalidate
004100090427     c                   exsr      FGT_200
004200090109      *
004300090427      *  Altrimenti deve aggiornare con 0 la data TGTDTS  invece che con 20090501
004400090109      *------------------
004500090427     c                   z-add     tgtdts        savdts            8 0
004600090427      *
004700090427     c                   if        tutte = 'OK' and almeno_uno = 'S'
004800090429      * rilascia il record
004900090429     c                   update    fitgt000
005000090429      *
005100090427     c                   clear                   ficn02ds
005200090427     c                   eval      cn02sml = tgtsml
005300090427     c                   eval      cn02ana = 'A'
005400090427     c                   eval      cn02pdr = tgtPDR
005500090427     c                   eval      cn02prg = tgtPRG
005600090427     c                   eval      cn02div = tgtDIV
005700090427      *
005800090427     c                   call      'TNVRA19R1'
005900090427     c                   parm                    ficn02ds
006000090427      *
006100090427     c                   else
006200090427      *
006300090427     c                   clear                   tgtdts
006400090427     c                   clear                   tgtpcar
006500090427     c                   clear                   tgtsoc
006600090427     c                   clear                   tgtcdf
006700090427     c                   clear                   TGTTMPC
006800090427     c                   clear                   TGTTMPS
006900090427     c                   clear                   tgtpkl
007000090427     c                   update    fitgt000
007100090429      *
007200090429      *  Azzera anche i relativi FGT
007300090429     c                   exsr      azzera_DTS_fgt
007400090427      *
007500090427     c                   end
007600090109      *------------------
007700090424      *  scrive il dettaglio su un file
007800090424     c                   exsr      wri_DETTA
007900090427     c                   endIF
008000081110      *
008100090427     c                   read      fitgt01l
008200081110     C                   ENDDO
008300090429      *------------------
008400090429      *
008500090429      *  Toglie dal TGT la Data di convalida per gli AUT che sono esclusi
008600090429      *   da AUTOFATTURAZIONE
008700090429      *
008800090429     C/EXEC SQL
008900090429     C+   update fitgt00f set TGTdts = 0
009000090429     C+   where tgtpdr in
009100090429     C+    (select apdpdr from fiapd00f
009200090429     C+   where
009300090429     C+     apdatb= ' ' and apdtip='A' and apdpdd='S')
009400090429     C/END-EXEC
009500090429      *
009600090429      *
009700090429      *
009800090429      *------------------
009900081110     C     FINE          TAG
010000081110     c                   except    finest
010100081110     c                   return
010200081209     C*------------------------------------------------------------------------*
010300081209     C* *INZSR - OPERAZIONI INIZIALI
010400081209     C*------------------------------------------------------------------------*
010500081209     C     *INZSR        BEGSR
010600081209      *
010700081209     C     *ENTRY        PLIST
010800081209     C                   PARM                    AGGIORNA          1
010900090427     C*
011000090427     c     Key_FGT       KLIST
011100090427     c                   kfld                    tGTPDR
011200090427     c                   kfld                    tGTSML
011300090401     C*
011400081209     c                   clear                   esito
011500081209      *
011600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
011700081209     C                   clear                   UDATE_8           8 0
011800081209     C                   MOVEL     *DATE         UDATE_8
011900090109      *
012000081209     c                   endSR
012100090109      *---------------------------------------------------------------*
012200090109      *  Scrive Testate e Dettaglio
012300090109      *---------------------------------------------------------------*
012400090427     c     FGT_200       begSR
012500090109     C*
012600090427     c                   move      'OK'          tutte             2
012700090427     c                   move      *blank        almeno_uno        1
012800090427     C*
012900090429     c     Key_FGT       setll     fifgt01L
013000090429     c     Key_FGT       reade(n)  fifgt01L
013100090427     c                   Dow       not %EoF(fifgt01l)
013200090423     C*
013300090427     c                   if        fgtprg = 200
013400090427     c                   move      'S'           almeno_uno        1
013500090427     c                   if        fgtdts = 0
013600090427     c                   clear                   tutte
013700090427     c                   leave
013800090427     c                   end
013900090427     c                   end
014000090423      *
014100090429     c     Key_FGT       reade(n)  fifgt01L
014200090109     c                   endDo
014300090109     C*
014400090109     C*
014500090109     c                   endSR
014600090429      *---------------------------------------------------------------*
014700090429      *  Azzera FGTDTS
014800090429      *---------------------------------------------------------------*
014900090429     c     azzera_DTS_fgtbegSR
015000090429     C*
015100090429     c     Key_FGT       setll     fifgt01L
015200090429     c     Key_FGT       reade     fifgt01L
015300090429     c                   Dow       not %EoF(fifgt01l)
015400090429     C*
015500090429     c                   if        fgtprg = 200
015600090429     c                   eval      fgtdts = 0
015700090429     c                   end
015800090429      *
015900090429     c                   update    fifgt000
016000090429      *
016100090429     c     Key_FGT       reade     fifgt01L
016200090429     c                   endDo
016300090429     C*
016400090429     C*
016500090429     c                   endSR
016600081211     C*---------------------------------------------------------------*
016700090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
016800090424      *---------------------------------------------------------------*
016900090424     c     WRI_DETTA     begSR
017000090424     C*
017100090427     c                   eval      WSFSML  =  tgtSML
017200090427     c                   eval      WSFPDR  =  tgtPDR
017300090427     c                   eval      WSFTSR  =  *blank
017400090427     c                   eval      WSFCTR  =  0
017500090427     c                   eval      WSFPRG  = tgtPRG
017600090427     c                   eval      WSFDDT  = tgtDDT
017700090427     c                   eval      WSFDST  = tgtDST
017800090427     c                   eval      WSFDTS  = savDTS
017900090427     c                   eval      WSFDCN  = tgtDCN
018000090427     c                   eval      WSFREC  = tGT_REC
018100090427     c                   eval      WSFDECOR= 0
018200090427     c                   eval      WSFSCAD = 0
018300090427     c                   eval      WSFPROGR= 'TNVRA19R'
018400090424     c**
018500090424     c                   write     WSFGT000
018600090424     C*
018700090424     c                   endSR
018800090424     C*---------------------------------------------------------------*
018900081110     OQSYSPRT   E            TESTA            02
019000081110     O                                           16 'Simul'
019100081110     O                                           25 'Autista'
019200081110     O                                           45 'Progr'
019300081110     O                                           55 'Data_Dec'
019400081110     O                                           65 'Data_Sca'
019500081110     O                                           75 'Data_Stp'
019600081110     O                                           85 'Data_Cnv'
019700081211     O                                          115 'N_Rec_FGT'
019800081110     O          E            DETta       1
019900090427     O                       tgtSML              14
020000090427     O                       tgtPDR        z     25
020100090427     O                       tgtPRG        z     43
020200090427     O                       tgtDDT        z     55
020300090427     O                       tgtDST        z     65
020400090427     O                       tgtDTS        z     75
020500090427     O                       tgtDCN        z     85
020600090427     o                       TGT_REC       z    115
020700081110      ****
020800081110     o          E            FINEST      2
020900090427     o                                           30 '*** TNVRA19R ***'
021000090102     o                                           59 '***   FINE  STAMPA  ***'
021100090109     o                                          + 9 '***  SUPER TARIFFA  ***'
