000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081110     H* TNVR1AR *----------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800081218     Ffifgt01L  iF   E           K DISK
000900081218     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001000081222     F                                     usropn
001100081216     Fwffgt00f  o  a E           K DISK    prefix(WF:2)
001200081211      *
001300081211     Ffifpt01L  uF   E           K DISK    usropn
001400081211     Ffifpd01L  uF   E           K DISK    usropn
001500081211      *
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001900081110     d                                     prefix(STP:3)
002000081110     D elimina_riferimento...
002100081110     D                 s              2a
002200081110     D esito           s             10i 0
002300081110     D FGT_REC         s              9s 0
002400931202     D KPJBA         E DS
002500081230      *
002600081230     D returnDDT       s                   like(FgtDDT)
002700081230     D returnDST       s                   like(FgtDST)
002800081230      *
002900900521     C*****************************************************************
003000081209     C                   SETON                                        LR
003100020418     C*
003200081110     C/EXEC SQL DECLARE A1 CURSOR FOR
003300081110     C+ SELECT rrn(fifgt00f), fifgt00f.* FROM
003400090429     C+ edpabcur/fifgt00f
003500090429     C+  WHERE FGTTSR in ('R', 'C', 'G') and FGTRAP >0
003600090102     C+ and fgtATB = ' '
003700090102     C+ ORDER BY
003800081110     C+ FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
003900081110     C/END-EXEC
004000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
004100081110     c                   if        sqlCODE < 0
004200081110     c                   eval      esito = Sqlcode
004300081110     c                   dump(a)
004400081110     c                   return
004500081110     c                   end
004600081110     C/EXEC SQL
004700081110     C+ OPEN A1
004800081110     C/END-EXEC
004900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
005000081110     c                   if        sqlCODE < 0
005100081110     c                   eval      esito = Sqlcode
005200081110     c                   dump(a)
005300081110     c                   return
005400081110     c                   else
005500081110     c                   except    testa
005600081110     c                   end
005700081110      *
005800081209      * resto nel ciclo finchè....
005900081110      *
006000081110     C                   DOU       SqlCod < 0
006100081110      *
006200081110      * lettura cursore
006300081110     C/EXEC SQL
006400081110     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
006500081110     C/END-EXEC
006600081110      *  EoF
006700081110     C                   if        SqlCod = 100
006800081110     c                   Leave
006900081110      *  x errori
007000081110     C                   ElseIf    SqlCod < 0
007100081110     c                   eval      esito = Sqlcode
007200081110     c                   dump(a)
007300081110     c                   leave
007400081110     C                   ENDif
007500081110      *
007600081210     c                   clear                   eseguito
007700081222     c                   clear                   verifica
007800090102     c                   clear                   errore           10
007900081110      *
008000081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
008100081110     c                   if        Aggiorna = 'S'
008200081110      *
008300081216      *  imposta il file di work a pro-memoria delle tariffe di riferimento
008400081216      *   da eliminare in seguito
008500081216     c                   clear                   wffgt000
008600081217     c                   z-add     STPPDR        wfTPDR
008700081217     c                   move      STPSML        wfTSML
008800081217     c                   move      STPTSR        wfTTSR
008900081217     c                   move      STPCTR        wfTCTR
009000081217     c                   z-add     STPPRG        wfTPRG
009100081217     c                   z-add     STPRAP        wfTRAP
009200081217     c                   move      STPRCT        wfTRCT
009300090109     c                   z-add     STPDDT        wfTDDT
009400090109     c                   z-add     STPDST        wfTDST
009500081216      *
009600081211      * Se erano presenti records su FPT e FPD anche annullati li elimina
009700081211      *  prima di scrivere con il programma seguente
009800081211     C                   EXSR      PULISCE
009900081211      *
010000081110     C     fgt01K        chain     fifgt01l
010100081211     c                   if        %Found(fifgt01l)
010200081230      *
010300081230     c                   clear                   returnDST
010400081121      *
010500081121      * duplica la tariffa da tariffa di riferimento
010600090109     c                   call      'TNVRA11R'
010700081121     c                   parm                    FGTPDR
010800081121     c                   parm                    FGTSML
010900081121     c                   parm                    FGTTSR
011000081121     c                   parm                    FGTCTR
011100081121     c                   parm                    FGTPRG
011200081121     c                   parm                    fgtRAP
011300081121     c                   parm                    fgtRCT
011400081210     c                   parm                    fgtDDT
011500081210     c                   parm                    fgtDST
011600081230     c                   parm                    returnDST
011700081209     c                   parm                    fgt_REC
011800081209     c                   parm                    eseguito          1
011900081222     c                   parm                    verifica          1
012000081222      *
012100081222     c                   if        verifica <> *blank
012200081222     c                   eval      WFtNOTE = 'Da Verificare'
012300090102     c                   else
012400090102      *
012500090102     c                   if        eseguito = '1'
012600090102     c                   eval      WFtNOTE = 'Errore scrittura'
012700090102      *
012800090102      *  Aggiunte nuove testate poichè le tariffe di riferimento
012900090102      *   avevano più periodi contigui.
013000090102     c                   elseIf    eseguito = '2'
013100090102     c                   eval      WFtNOTE = 'Scritte + Testate'
013200090102      *
013300090102      *  Nessuna tariffa di riferimento trovata
013400090102     c                   elseIf    eseguito = '3'
013500090102     c                   eval      WFtNOTE  = 'Nessun riferimento'
013600090102     c                   endIf
013700090102      *
013800090102     c                   end
013900081222      *
014000081222     c                   write     wffgt000
014100081121      *
014200081121      *  Solo se non ci sono stati errori:
014300081218      *   deve eliminare i puntamenti del codice di riferimento
014400081230     c*******            if        eseguito = *blank
014500081222      *
014600081222     c                   open      fifgt00f
014700081218     c     fgt_REC       chain     FIFGT00F
014800081218     c                   if        %Found(FIFGT00F)
014900090102      *
015000090114     c*******            eval      fgtDCV = 'Da ' + %char(fgtRAP) + '/' +
015100090114     c*******                      %char(fgtRCT)
015200090102      *
015300090102      *  Tariffa di riferimento non trovata
015400090102     c                   If        eseguito = '3'
015500090102     c                   move      '*N'          fgtDCV
015600090421     c                   if        fgtDST > 20090430
015700090421     c                   z-add     20090430      fgtDST
015800090115     c                   end
015900090102     c                   endIf
016000090102      *
016100090102      *
016200081110     c                   clear                   fgtrap
016300081110     c                   clear                   fgtrct
016400081230      *
016500081230      *  su + periodi
016600090102     c                   if        fgtDST > returnDST and returnDST > 0
016700081230     c                   eval      fgtDST = returnDST
016800081230     c                   end
016900081230      *
017000081218     c                   update    fgtFisico
017100081121     c                   endif
017200081230      *
017300081222     c                   close     fifgt00f
017400081222      *
017500081230     c*******            endif
017600081121      *
017700081110     c                   endIf
017800081110     c                   endIf
017900081210      *
018000081230     c                   if        eseguito = '1'
018100081230     c                   movel(p)  'Errore'      errore
018200081210     c                   endIf
018300081230      *
018400081230      *  Aggiunte nuove testate poichè le tariffe di riferimento
018500081230      *   avevano più periodi contigui.
018600081230     c                   if        eseguito = '2'
018700081230     c                   movel(p)  '+ Testate'   errore
018800081230     c                   endIf
018900081210      *
019000090102     c                   if        eseguito = '3'
019100090102     c                   movel(p)  'Nessun Rif'  errore
019200090102     c                   endIf
019300090102      *
019400081210      *  se sono qui ho letto un record correttamente
019500081210     c   Of              except    testa
019600081210     c                   except    detta
019700081110      *
019800081110     C                   ENDDO
019900081110     C/EXEC SQL
020000081110     C+ Close A1
020100081110     C/END-EXEC
020200081110     C                   if        Esito < 0
020300081110     c                   return
020400081110     c                   endIF
020500081110     C*
020600081110     C     FINE          TAG
020700081110     c                   except    finest
020800081110     c                   return
020900081209     C*------------------------------------------------------------------------*
021000081209     C* *INZSR - OPERAZIONI INIZIALI
021100081209     C*------------------------------------------------------------------------*
021200081209     C     *INZSR        BEGSR
021300081209      *
021400081209     C     *ENTRY        PLIST
021500081209     C                   PARM                    AGGIORNA          1
021600081209     C*
021700081209     C     fgt01K        kLIST
021800081211     C                   kfld                    stpPDR
021900081211     C                   kfld                    stpSML
022000081211     C                   kfld                    stpTSR
022100081211     C                   kfld                    stpCTR
022200081211     C                   kfld                    stpPRG
022300081209     C*
022400081209     c                   clear                   esito
022500081209      *
022600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
022700081209     C                   clear                   UDATE_8           8 0
022800081209     C                   MOVEL     *DATE         UDATE_8
022900081209     C*
023000081209     c                   endSR
023100081110     C*---------------------------------------------------------------*
023200081211     c     PULISCE       begSR
023300081211     C*
023400081211     c                   open      fifpt01l
023500081211     C     fgt01K        setll     fifpt01l
023600081211     C     fgt01K        reade     fifpt01l
023700081211     c                   dow       not %EoF(fifpt01l)
023800081211     c                   delete    fifpt000
023900081211     C     fgt01K        reade     fifpt01l
024000081211     c                   enddo
024100081211     c                   close     fifpt01l
024200081211     C*
024300081211     c                   open      fifpd01l
024400081211     C     fgt01K        setll     fifpd01l
024500081211     C     fgt01K        reade     fifpd01l
024600081211     c                   dow       not %EoF(fifpd01l)
024700081211     c                   delete    fifpd000
024800081211     C     fgt01K        reade     fifpd01l
024900081211     c                   enddo
025000081211     c                   close     fifpd01l
025100081211     C*
025200081211     c                   endSR
025300081211     C*---------------------------------------------------------------*
025400081110     OQSYSPRT   E            TESTA            02
025500081110     O                                           16 'Simul'
025600081110     O                                           25 'Autista'
025700081110     O                                           30 'TSR'
025800081110     O                                           38 'Tariffa'
025900081110     O                                           45 'Progr'
026000081110     O                                           55 'Data_Dec'
026100081110     O                                           65 'Data_Sca'
026200081110     O                                           75 'Data_Stp'
026300081110     O                                           85 'Data_Cnv'
026400081110     O                                           95 'Aut_Rif'
026500081110     O                                          105 'Tar_Rif'
026600081211     O                                          115 'N_Rec_FGT'
026700081110     O          E            DETta       1
026800081110     O                       stpSML              14
026900081110     O                       stpPDR        z     25
027000081110     O                       stpTSR              29
027100081110     O                       stpCTR              36
027200081110     O                       stpPRG        z     43
027300081110     O                       stpDDT        z     55
027400081110     O                       stpDST        z     65
027500081110     O                       stpDTS        z     75
027600081110     O                       stpDCN        z     85
027700081110     O                       stpRAP        z     95
027800081110     O                       stpRCT        z    103
027900081211     o                       FGT_REC       z    115
028000081230     O                       errore             130
028100081110      ****
028200081110     o          E            FINEST      2
028300090109     o                                           30 '*** TNVRA10R ***'
028400090102     o                                           59 '***   FINE  STAMPA  ***'
028500090102     o                                          + 9 '***  TAR. RIFERIMENTO ***'
