000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081121     H* TNVR11AR *---------------------------------------------------*
000300081121     H*        Duplica le Tariffe                                    *
000400000000     H*--------------------------------------------------------------*
000500081222     Ffifgt01L  iF   E           K DISK
000600081222     F                                     INFDS(FGTDS)
000700081210     Ffifpt01L  iF   E           K DISK
000800081230     Ffifpt00F  o  a E           K DISK    RENAME(FIFPT000:FIFPTout)
000900081210     F                                     prefix(tes:3)
001000081210     Ffifpd01L  iF   E           K DISK
001100081230     Ffifpd00F  o  a E           K DISK    RENAME(FIFPD000:FIFPDout)
001200081210     F                                     prefix(det:3)
001300081222     D FGTDS           DS
001400081222     D  NrecFGTFis           397    400B 0
001500081110      *
001600081121     D FGT_DS        e ds                  EXTNAME(FIFGT00F)
001700081110      *
001800931202     D KPJBA         E DS
001900081124      *
002000081209     D Data_Oggi       s              8s 0
002100081209      *
002200081209     D prmPDR          s                   like(FgtPDR)
002300081124     D prmSML          s                   like(FgtSML)
002400081124     D prmTSR          s                   like(FgtTSR)
002500081124     D prmCTR          s                   like(FgtCTR)
002600081124     D prmPRG          s                   like(FgtPRG)
002700081124     D prmRAP          s                   like(FgtRAP)
002800081124     D prmRCT          s                   like(FgtRCT)
002900081210     D prmDDT          s                   like(FgtDDT)
003000081210     D prmDST          s                   like(FgtDST)
003100081211     D prmNREC         s              9s 0
003200081229      *
003300081229     D newPRGtes       s                   like(FgtPRG)
003400081230     D savPRG          s                   like(FgtPRG)
003500081229     D dataDDT         s                   like(FgtDDT)
003600081229     D dataDST         s                   like(FgtDST)
003700081230     D sv_FPTPDR       s                   like(FptPDR)
003800081230     D sv_FPTSML       s                   like(FptSML)
003900081230     D sv_FPTTSR       s                   like(FptTSR)
004000081230     D sv_FPTCTR       s                   like(FptCTR)
004100081230     D sv_FPTPRG       s                   like(FptPRG)
004200081230      *
004300081230     D returnDDT       s                   like(FgtDDT)
004400081230     D returnDST       s                   like(FgtDST)
004500900521     C*****************************************************************
004600081209      *   Crea le tariffe direttamente sull'autista fotocopiando
004700081209      *    la tariffa di riferimento.
004800081209     C*****************************************************************
004900000000     C     *ENTRY        PLIST
005000081124     C                   PARM                    prmPDR
005100081124     C                   PARM                    prmSML
005200081124     C                   PARM                    prmTSR
005300081124     C                   PARM                    prmCTR
005400081124     C                   PARM                    prmPRG
005500081124     C                   PARM                    prmRAP
005600081124     C                   PARM                    prmRCT
005700081210     c                   parm                    prmDDT
005800081210     c                   parm                    prmDST
005900081230     c                   parm                    returnDST
006000081211     C                   PARM                    prmNREC
006100081121     C                   PARM                    esito             1
006200081222     C                   PARM                    verifica          1
006300081124      *
006400081124      * Aggancia la tariffa di riferimento
006500081230     C     parm_KEY      kLIST
006600081124     C                   KFLD                    prmRAP
006700081124     C                   KFLD                    prmSML
006800081124     C                   KFLD                    prmTSR
006900081124     C                   KFLD                    prmRCT
007000081124      *
007100081209     C     fgt_KEY       kLIST
007200081209     C                   KFLD                    fgtPDR
007300081209     C                   KFLD                    fgtSML
007400081209     C                   KFLD                    fgtTSR
007500081209     C                   KFLD                    fgtCTR
007600081209     C                   KFLD                    fgtPRG
007700081209      *
007800081110     C                   SETON                                        LR
007900081218      * se non ha trovato il progressivo necessario x la tariffa di riferimento
008000081218      *  deve segnalare l'errore.
008100090102     C                   clear                   esito
008200090102     c                   MOVE      'N'           Riferimento       1
008300081209     c                   move      *date         Data_Oggi
008400081230     C                   clear                   prima_volta       1
008500081209      *
008600081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
008700081210      *    poichè non essendo gestito il progressivo sulle tariffe di
008800081210      *     riferimento occorre testare le date di decorrenza - scadenza
008900081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
009000081210      *
009100081209      *  Testata Dettagli
009200081230     c     parm_KEY      setll     fiFGT01l
009300081230     c     parm_KEY      reade     fiFGT01l
009400081209      *
009500081209     c                   dow       not %EoF(fiFGT01l)
009600081230      *
009700081210     c                   if        FGTatb = *blank
009800081230      *
009900081230      * Tariffa di riferimento del periodo giusto
010000081210     c                   if        prmDDT >= FGTddt and prmDDT <= FGTdst or
010100090102     c                             prmDST >= FGTddt and prmDST <= FGTdst or
010200090102     c                             prmDDT <= FGTddt and prmDST >= FGTdst
010300081230      **
010400081230      **  Data del periodo riferito da riportare sul record di testata aggiornato
010500081230      * >>>>>
010600081230     C                   if          prima_volta = *blank
010700081230     C                   eval      prima_volta = 'S'
010800090102     c                   z-add     prmPRG        savPRG
010900081230      **
011000081230     c                   if        prmDST >  fgtDST
011100081230     c                   eval      returnDST = fgtDST
011200081230     c                   end
011300081230      **
011400090102     c                   else
011500090102     c                   add       1             savPRG
011600090102     c                   end
011700081230      * >>>>>
011800081222     c                   reset                   nuova_testata     1
011900081218     c                   exsr      testata_FPT
012000081218     c                   exsr      dettaglio_FPD
012100081222     c                   end
012200081230      *
012300081218     c                   endif
012400081218      *
012500081230     c     parm_KEY      reade     fiFGT01l
012600081218     c                   enddo
012700081218      *
012800090102      **
012900090102      *  Non ha trovato nessuna tariffa di riferimento
013000090102     c                   if        esito = *blank and Riferimento = 'N'
013100090102     C                   move      '3'           esito
013200090102     c                   end
013300081210     C*
013400081110     C     FINE          TAG
013500081110     c                   return
013600081110     C*---------------------------------------------------------------*
013700081218      *   Copia la testata
013800081218     C*---------------------------------------------------------------*
013900081218     c     testata_FPT   begsr
014000081229      *
014100090102     c                   clear                   solo1volta        1
014200090102      *
014300081218      *  Testata tariffe
014400081218     c     FgT_KEY       setll     fiFPT01l
014500081218     c     FgT_KEY       reade     fiFPT01l
014600081218      *
014700081218     c                   dow       not %EoF(fiFPT01l)
014800081230      *  se non annullato
014900081230     c                   if        fptATB = *blank
015000081229      *
015100081229      *  e una pulizia prima della scrittura del progressivo
015200081229      *   x verificarne l'esistenza.
015300081229     c                   clear                   newPRGtes
015400090102     c                   MOVE      'S'           Riferimento       1
015500081218      *
015600081218      *  Campi chiave dell'autista specifico
015700081218     c                   clear                   fiFPTout
015800081218     c                   z-add     prmPDR        tesPDR
015900081218     c                   z-add     prmCTR        tesCTR
016000090102     c                   z-add     savPRG        tesPRG
016100081218     c                   move      prmSML        tesSML
016200081218     c                   move      prmTSR        tesTSR
016300081218      *
016400081218     c                   move      fptCTD        tesCTD
016500081218     c                   move      fptTAT        tesTAT
016600081218     c                   move      fptTPG        tesTPG
016700081218     c                   move      fptTSM        tesTSM
016800081218     c                   move      fptTPP        tesTPP
016900081218     c                   z-add     fptATA        tesATA
017000081218     c                   z-add     fptARL        tesARL
017100081218     c                   z-add     fptARF        tesARF
017200081218     c                   z-add     fptARO        tesARO
017300081218     c                   z-add     fptRPV        tesRPV
017400081218     c                   move      fptFLR        tesFLR
017500081218     c                   move      fptFTR        tesFTR
017600081218     c                   z-add     fptDTR        tesDTR
017700081218      *
017800090102     c***  riscrive_T    tag
017900090102     c                   write     fiFPTout                             99
018000090102     c   99              eval      esito = '1'
018100090102      *
018200081229      *   se si è scritto un nuovo progressivo occorre scrivere la relativa testata
018300090102      **
018400090102      **  Deve scrivere una nuova TESTATA
018500090102     C                   If        savPRG <> prmPRG    and
018600090102     c                                  solo1volta <> 'S'
018700090102      **
018800081229      ** ----------------------------------------------------------------------
018900081229      **  Occorre demandare esternamente la scrittura della testata su un altro file
019000081229      **   di work identico al FIFGT per non andare ad intralciare il ciclo di
019100081229      **   lettura delle testate di riferimento in questo programma.....poi,
019200081229      **   alla fine di tutto, il file di work verrà copiato sul FIFGT in ADD.
019300081229      **
019400081229      ** esempio: tar.  C/300 riferita all'AUT xxx
019500081229      **   tempo:
019600081229      **     Date sulla linea temporale della tariffa e di quella di riferimento
019700081229      **     >>>--------1----2----------3--4---------------5---------->>>>
019800081229      **
019900081229      **   tariffa AUT       DDT            prm                  DST
020000081229      **    da PARM          |---------------------------------- oo
020100081229      **                    (2)
020200081229      **                DDT           DST DDT             DST
020300081229      **  Tariffa di    (1)    fgt 1°  (3)(4)   fgt 2°    (5)
020400081229      **           RIFER |---------------|-----------------|
020500081229      **
020600081229      ** Le 2 testate avranno i seguenti periodi per l'autista
020700081229      **  1° periodo :  DDT = (2)    e   DST = (3)
020800081229      **  2° periodo :  DDT = (4)    e   DST = (5)
020900081229      **
021000081229      ** ----------------------------------------------------------------------
021100081229      ** Imposta i limiti della nuova testata da scrivere
021200081229      **
021300081229     c                   if        prmDDT >= fgtDDT
021400081229     c                   eval        dataDDT = prmDDT
021500081229     c                   else
021600081229     c                   eval        dataDDT = fgtDDT
021700081229     c                   end
021800081229      **
021900081230     c                   if        prmDST <= fgtDST
022000081229     c                   eval        dataDST = prmDST
022100081229     c                   else
022200081229     c                   eval        dataDST = fgtDST
022300081229     c                   end
022400081229      **
022500090102      **
022600081229     c                   call      'TNVR12AR'
022700081229     c                   parm                    prmNREC
022800090102     c                   parm                    savPRG
022900081229     c                   parm                    dataDDT
023000081229     c                   parm                    dataDST
023100090102      **
023200090102     c                   eval      solo1volta = 'S'
023300081229      **
023400081230      *  Ha scritto delle nuove testate
023500081230     C                   move      '2'           esito
023600081229     c                   end
023700081229      **
023800081218     c                   endif
023900081218      *
024000081218     c     FgT_KEY       reade     fiFPT01l
024100081218     c                   enddo
024200081230      *
024300081218     c                   endsr
024400081218     C*---------------------------------------------------------------*
024500081218      *   Copia il dettaglio
024600081218     C*---------------------------------------------------------------*
024700081218     c     dettaglio_FPD begsr
024800081218      *
024900081218      *  Dettaglio tariffe
025000081218     c     FgT_KEY       setll     fiFPD01l
025100081218     c     FgT_KEY       reade     fiFPD01l
025200081218      *
025300081218     c                   dow       not %EoF(fiFPD01l)
025400081218      *  se non annullato
025500081218     c                   if        fpdATB = *blank
025600081218      *
025700081218      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
025800081218      *    poichè non essendo gestito il progressivo sulle tariffe di
025900081218      *     riferimento occorre testare le date di decorrenza - scadenza
026000081218      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
026100081218      *
026200081218      *  Campi chiave dell'autista specifico
026300081218     c                   clear                   fiFPDout
026400081218     c                   z-add     prmPDR        detPDR
026500081218     c                   z-add     prmCTR        detCTR
026600081230      *
026700090102     c                   z-add     savPRG        detPRG
026800081218     c                   move      prmSML        detSML
026900081218     c                   move      prmTSR        detTSR
027000090102      *
027100081218     c                   move      fpdCTD        detCTD
027200081218     c                   move      fpdCAP        detCAP
027300081218     c                   z-add     fpdSGL        detSGL
027400081218     c                   z-add     fpdITR        detITR
027500081218     c                   z-add     fpdMIN        detMIN
027600081218     c                   z-add     fpdMAX        detMAX
027700081218     c                   move      fpdAIN        detAIN
027800081218     c                   move      fpdFTR        detFTR
027900081218     c                   z-add     fpdDTR        detDTR
028000081218      *
028100081230     c                   write     fiFPDout
028200081218     c                   endif
028300081218      *
028400081218     c     FgT_KEY       reade     fiFPD01l
028500081218     c                   enddo
028600081218      *
028700081218     c                   endsr
028800081218     C*---------------------------------------------------------------*
