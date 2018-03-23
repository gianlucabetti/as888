000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120612     H* TNVRABT5R*---------------------------------------------------*
000300081110     H*                                                              *
000400120604     H* aggancia dalle tariffe i TRAZIONISTI in essere e collega     *
000500120604     H*  i contratti TRAZIONISTI ex 9999999                          *
000600081209     H*                                                              *
000700120604     H*   Deve aggiornare il Numero Contratto e la filiale           *
000800120604     H*   solo se è un trazionista con nr. > 9000000.                *
000900000000     H*--------------------------------------------------------------*
001000120604     Fanrco98j  IF   E           K DISK
001100120604      *
001200120604     Ffiadt00f  uF a E             DISK    commit
001300111201     FwfADTXXF  o  a E             DISK
001400111201     FwxADT00S  o  a E             DISK
001500120604      *
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800111122     D KPJBA         E DS
001900120604     D DSstpADT      E DS                  ExtName(FIADT00F)
002000081110     d                                     prefix(STP:3)
002100120604      *
002200120604     D stpADT_REC      s              9s 0
002300081230      *
002400111123     D esito           s              9s 0
002500111123     D esito_ALF       s              9a
002600120604      *
002700120604     d dsAIATS       e ds                  extname(AIATS00F)
002800120604     D partraz         DS
002900120604     d  parsoc                        3  0
003000120604     D  pariva                       16
003100120604     D  pardtin                       8s 0
003200120604     D  pardtfi                       8s 0
003300120604     d  esitotraz                     1
003400120604     d  errmsg                       75
003500090109      *
003600111123     D DATA_eur        S               D   DATFMT(*eur)
003700111123     D DATA_iso        S               D   DATFMT(*iso)
003800111123     C*------------------------------------------------------------------------*
003900111123     C*        - OPERAZIONI INIZIALI
004000111123     C*------------------------------------------------------------------------*
004100111123     C     *ENTRY        PLIST
004200111123     C                   PARM                    AGGIORNA          1
004300111123     C                   PARM                    ESITO_ALF
004400111123     C*
004500120604     C     Krco          KLIST
004600120604     C                   KFLD                    stpcsf
004700120604     C                   KFLD                    SNatura           1
004800120604     C                   KFLD                    stpcdf
004900120604     c                   eval      SNatura = 'F'
005000120604     C*
005100111123     C     AGGIORNA      comp      ' '                                    44
005200111123      *
005300111123     c                   clear                   esito
005400111123     c                   clear                   esito_ALF
005500111123     c                   time                    datetime         14 0
005600111123     c                   move      datetime      data              8 0
005700111123     c                   movel     datetime      ora               6 0
005800111123      *
005900111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006000111123     C                   clear                   UDATE_8           8 0
006100111123     C                   MOVEL     *DATE         UDATE_8
006200111201      *
006300081209     C                   SETON                                        LR
006400090423     C*=================================================================
006500120604      **  Sono le Tariffe da legare ai TRAZIONISTI
006600090423     C*=================================================================
006700111201      *   TESTATE AFFL.DEFL.
006800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006900111201     C+   SELECT rrn(fiadt00f), fiadt00f.*
007000111201     C+     FROM fiadt00f
007100120612     C+    WHERE adtnrc between 9000000 and 9999998
007200120612     C+             and adtcsf ='219' and
007300120604     C+          digits(adtpdr) concat adttsr concat digits(adtprg)
007400120604     C+             NOT IN
007500120604     C+   (Select
007600120604     C+          digits(adtpdr) concat adttsr concat digits(adtprg)
007700120604     C+    from WFADTXXF)
007800120604     C+ order by ADTpdr
007900111122     C/END-EXEC
008000111122
008100111122     C*=====
008200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008300111122     C*=====
008400081110     c                   if        sqlCODE < 0
008500081110     c                   eval      esito = Sqlcode
008600111123     c                   move      esito         esito_ALF
008700081110     c                   dump(a)
008800081110     c                   return
008900081110     c                   end
009000081110     C/EXEC SQL
009100081110     C+ OPEN A1
009200081110     C/END-EXEC
009300111122     C*=====
009400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009500111122     C*=====
009600081110     c                   if        sqlCODE < 0
009700081110     c                   eval      esito = Sqlcode
009800111123     c                   move      esito         esito_ALF
009900081110     c                   dump(a)
010000081110     c                   return
010100081110     c                   else
010200111122      *
010300081110     c                   except    testa
010400111122      *
010500081110     c                   end
010600090109      * >>>>>>>
010700081209      * resto nel ciclo finchè....
010800081110     C                   DOU       SqlCod < 0
010900081110      * lettura cursore
011000081110     C/EXEC SQL
011100120604     C+ Fetch Next From A1 Into :stpADT_REC, :DSstpADT
011200081110     C/END-EXEC
011300111122     C*=====
011400111122      *  EoF
011500081110     C                   if        SqlCod = 100
011600081110     c                   Leave
011700111122     C*=====
011800081110      *  x errori
011900081110     C                   ElseIf    SqlCod < 0
012000081110     c                   eval      esito = Sqlcode
012100111123     c                   move      esito         esito_ALF
012200081110     c                   dump(a)
012300081110     c                   leave
012400081110     C                   ENDif
012500090428      *
012600111123      *----------------------------->>>>>
012700111123      *  se sono qui ho letto un record correttamente
012800090428      *----------------------------->>>>>
012900120605      *
013000111123     c                   clear                   errore           15
013100120604      *
013200120604      *  Con il TRULTRAZ controlla se è un trazionista veramente
013300120604      *
013400120604     C     Krco          CHAIN     anrco98j
013500120604     c                   if        not %Found(anrco98j)
013600120604      *
013700120604      *  Manca il Fornitore su PROJ.
013800120604     c                   if        errore  = *blank
013900120604     c                   eval        Errore = 'NO FORNITORE'                    <--- errore
014000120604     c                   end
014100120604      *
014200120604     c                   else
014300120604      **
014400120604     c* controllo se il fornitore è presente in archivio trazionisti
014500120604     c* e se sono un codice di A/D nel caso la società deve essere quella
014600120604     c* dei traini
014700120604ab   c                   move      'NO'          trazionista       2
014800120604     c                   clear                   partraz
014900120604     c                   clear                   dsAIATS
015000120612     c                   movel     stpCSF        parsoc
015100120612     c                   z-add     stpDDT        pardtin
015200120612     c                   z-add     stpDST        pardtfi
015300120604     c                   movel     sogpartiva    pariva
015400120604     c                   call      'TRULTRAZ'
015500120604     c                   parm                    partraz
015600120604     c                   parm                    dsAIATS
015700120604      ***
015800120604      ***  Se si tratta di un trazionista
015900120604     c                   if        esitotraz <>'1'
016000120604     c                   eval        Errore = esitotraz + ' da TRULTRAZ'        <--- errore
016100120604      ***
016200120604     c                   elseIF    esitotraz = '1'
016300120604      ***
016400120604ab   c                   move      'SI'          trazionista       2
016500120604      *----------------------------->>>>>
016600111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
016700111123      *----------------------------->>>>>
016800120604     c                   if         Aggiorna = 'S'
016900120604      *   Aggancia la TARRIFA
017000120604     C     stpADT_REC    chain     fiADT00F
017100120604     c                   if        %Found(fiADT00F)
017200120604     c                   z-add     atsNRC        adtNRC
017300120604     c                   z-add     046           adtFIL
017400111201     c                   update    fiADT000
017500111201     c                   end
017600111201     c                   endIF
017700090428      *----------------------------->>>>>
017800120604     c                   endif                                                  da EsitoTRAZ
017900120604      *
018000120604     c                   end
018100120604      *
018200120604      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
018300120604      *    se si fosse interrotto a metà
018400120604     c                   write     fiADTxxx
018500120604      *
018600111206      *----
018700111206     c   Of              except    testa
018800111206     c                   except    detta
018900111206      *
019000090424      *  scrive il dettaglio su un file
019100090424     c                   exsr      wri_DETTA
019200081110      *
019300081110     C                   ENDDO
019400081110     C/EXEC SQL
019500081110     C+ Close A1
019600081110     C/END-EXEC
019700081110     C                   if        Esito < 0
019800111123     c                   move      esito         esito_ALF
019900081110     c                   return
020000081110     c                   endIF
020100081110     C*
020200081110     C     FINE          TAG
020300081110     c                   except    finest
020400081110     c                   return
020500111122     C*---------------------------------------------------------------*
020600111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
020700111122      *---------------------------------------------------------------*
020800111122     c     WRI_DETTA     begSR
020900111122     C*
021000111201     c                   clear                   WSADT000
021100090424     C*
021200090424     c                   eval      WSFPDR  =  stpPDR
021300090424     c                   eval      WSFTSR  =  stpTSR
021400111201     c                   eval      WSFPRG  =  stpPRG
021500111201     c                   eval      WSFDDT  =  stpDDT
021600120608     c                   eval      WSFDST  =  stpDST
021700120608     c                   eval      WSFDCN  =  atsDEC
021800120608     c                   eval      WSFDTS  =  atsDFC
021900120605     c                   eval      WSFDUV  =  0
022000120605     c                   eval      WSFDRC  =  0
022100120608     c                   eval      WSFNRC  =  atsNRC
022200111201     c                   eval      WSFCDF  =  stpCDF
022300111201     c                   eval      WSFCSF  =  stpCSF
022400111201     c                   eval      WSFFIL  =  stpFIL
022500111201      *
022600111201     c                   eval      WSFFILE = 'FIADT00F'
022700120604     c                   eval      WSFREC  = StpADT_REC
022800120612     c                   eval      WSFPROGR= 'TNVRABT5R'
022900111123     c                   eval      WSFtime = datetime
023000090428     c**
023100090428     c                   eval      WSFNOTE = Errore
023200090424     c**
023300111201     c                   write     WSADT000
023400090424     C*
023500090424     c                   endSR
023600081211     C*---------------------------------------------------------------*
023700081110     OQSYSPRT   E            TESTA            02
023800120612     O                                           10 'TNVRABT5R'
023900081110     O                                           25 'Autista'
024000081110     O                                           30 'TSR'
024100081110     O                                           45 'Progr'
024200081110     O                                           55 'Data_Dec'
024300081110     O                                           65 'Data_Sca'
024400120605     O                                           73 'N.Contr.'
024500120605     O                                           83 'Dec_Cont'
024600120605     O                                           93 'Sca_Cont'
024700120605     O                                          106 'N_Rec_ADT'
024800120605     O                                          116 'N O T E'
024900111123     O                       data               132 '  /  /    '
025000111123     O          E            TESTA       1
025100111123     O                       ora                130 '  :  :  '
025200111123      *
025300081110     O          E            DETta       1
025400081110     O                       stpPDR        z     25
025500081110     O                       stpTSR              29
025600081110     O                       stpPRG        z     43
025700081110     O                       stpDDT        z     55
025800081110     O                       stpDST        z     65
025900120608     O                       atsNRC        z     73
026000120605     O                       atsDEC              83
026100120605     O                       atsDFC              93
026200120605     o                       stpadt_REC    z    106
026300120605     O                       errore             125
026400081110      ****
026500081110     o          E            FINEST      2
026600111123     o                                            1 ' '
026700120612     o                                          + 0 '*** TNVRABT5R ***'
026800120604     o                                          + 9 '***  TRAZI - AFF/DEF ***'
026900111123     o                                          + 9 '***   FINE  STAMPA  ***'
027000111123     O                       data               + 9 '  /  /    '
027100111123     O                       ora                + 3 '  :  :  '
027200111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
