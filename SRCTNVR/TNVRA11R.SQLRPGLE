000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090109     H* TNVRA11R *---------------------------------------------------*
000300081121     H*        Duplica le Tariffe                                    *
000400000000     H*--------------------------------------------------------------*
000500081222     Ffifgt01L  iF   E           K DISK
000600081222     F                                     INFDS(FGTDS)
000700081210     Ffifpt01L  iF   E           K DISK
000800081230     Ffifpt00F  o  a E           K DISK    RENAME(FIFPT000:FIFPTout)
000900081210     F                                     prefix(tes:3)
001000090119     FWFfpt00F  o  a E           K DISK    RENAME(FIFPT000:wfFPTout)
001100090119     F                                     prefix(tes:3)
001200081210     Ffifpd01L  iF   E           K DISK
001300090119     Ffifpd00F  o  a E           K DISK    RENAME(FIFPD000:FIFPDout)
001400081210     F                                     prefix(det:3)
001500090119     Fwffpd00F  o  a E           K DISK    RENAME(FIFPD000:wfFPDout)
001600090119     F                                     prefix(det:3)
001700081222     D FGTDS           DS
001800081222     D  NrecFGTFis           397    400B 0
001900081110      *
002000081121     D FGT_DS        e ds                  EXTNAME(FIFGT00F)
002100081110      *
002200931202     D KPJBA         E DS
002300081124      *
002400081209     D Data_Oggi       s              8s 0
002500081209      *
002600081209     D prmPDR          s                   like(FgtPDR)
002700081124     D prmSML          s                   like(FgtSML)
002800081124     D prmTSR          s                   like(FgtTSR)
002900081124     D prmCTR          s                   like(FgtCTR)
003000081124     D prmPRG          s                   like(FgtPRG)
003100081124     D prmRAP          s                   like(FgtRAP)
003200081124     D prmRCT          s                   like(FgtRCT)
003300081210     D prmDDT          s                   like(FgtDDT)
003400081210     D prmDST          s                   like(FgtDST)
003500081211     D prmNREC         s              9s 0
003600081229      *
003700081229     D newPRGtes       s                   like(FgtPRG)
003800081230     D savPRG          s                   like(FgtPRG)
003900081229     D dataDDT         s                   like(FgtDDT)
004000081229     D dataDST         s                   like(FgtDST)
004100081230     D sv_FPTPDR       s                   like(FptPDR)
004200081230     D sv_FPTSML       s                   like(FptSML)
004300081230     D sv_FPTTSR       s                   like(FptTSR)
004400081230     D sv_FPTCTR       s                   like(FptCTR)
004500081230     D sv_FPTPRG       s                   like(FptPRG)
004600081230      *
004700081230     D returnDDT       s                   like(FgtDDT)
004800081230     D returnDST       s                   like(FgtDST)
004900900521     C*****************************************************************
005000081209      *   Crea le tariffe direttamente sull'autista fotocopiando
005100081209      *    la tariffa di riferimento.
005200081209     C*****************************************************************
005300000000     C     *ENTRY        PLIST
005400081124     C                   PARM                    prmPDR
005500081124     C                   PARM                    prmSML
005600081124     C                   PARM                    prmTSR
005700081124     C                   PARM                    prmCTR
005800081124     C                   PARM                    prmPRG
005900081124     C                   PARM                    prmRAP
006000081124     C                   PARM                    prmRCT
006100081210     c                   parm                    prmDDT
006200081210     c                   parm                    prmDST
006300081230     c                   parm                    returnDST
006400081211     C                   PARM                    prmNREC
006500081121     C                   PARM                    esito             1
006600081222     C                   PARM                    verifica          1
006700081124      *
006800081124      * Aggancia la tariffa di riferimento
006900081230     C     parm_KEY      kLIST
007000081124     C                   KFLD                    prmRAP
007100081124     C                   KFLD                    prmSML
007200081124     C                   KFLD                    prmTSR
007300081124     C                   KFLD                    prmRCT
007400081124      *
007500081209     C     fgt_KEY       kLIST
007600081209     C                   KFLD                    fgtPDR
007700081209     C                   KFLD                    fgtSML
007800081209     C                   KFLD                    fgtTSR
007900081209     C                   KFLD                    fgtCTR
008000081209     C                   KFLD                    fgtPRG
008100081209      *
008200081110     C                   SETON                                        LR
008300081218      * se non ha trovato il progressivo necessario x la tariffa di riferimento
008400081218      *  deve segnalare l'errore.
008500090102     C                   clear                   esito
008600090102     c                   MOVE      'N'           Riferimento       1
008700081209     c                   move      *date         Data_Oggi
008800081230     C                   clear                   prima_volta       1
008900081209      *
009000081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
009100081210      *    poichè non essendo gestito il progressivo sulle tariffe di
009200081210      *     riferimento occorre testare le date di decorrenza - scadenza
009300081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
009400081210      *
009500081209      *  Testata Dettagli
009600081230     c     parm_KEY      setll     fiFGT01l
009700081230     c     parm_KEY      reade     fiFGT01l
009800081209      *
009900081209     c                   dow       not %EoF(fiFGT01l)
010000081230      *
010100081210     c                   if        FGTatb = *blank
010200081230      *
010300081230      * Tariffa di riferimento del periodo giusto
010400081210     c                   if        prmDDT >= FGTddt and prmDDT <= FGTdst or
010500090102     c                             prmDST >= FGTddt and prmDST <= FGTdst or
010600090102     c                             prmDDT <= FGTddt and prmDST >= FGTdst
010700081230      **
010800081230      **  Data del periodo riferito da riportare sul record di testata aggiornato
010900081230      * >>>>>
011000081230     C                   if          prima_volta = *blank
011100081230     C                   eval      prima_volta = 'S'
011200090102     c                   z-add     prmPRG        savPRG
011300081230      **
011400081230     c                   if        prmDST >  fgtDST
011500081230     c                   eval      returnDST = fgtDST
011600081230     c                   end
011700081230      **
011800090102     c                   else
011900090102     c                   add       1             savPRG
012000090102     c                   end
012100081230      * >>>>>
012200081222     c                   reset                   nuova_testata     1
012300081218     c                   exsr      testata_FPT
012400081218     c                   exsr      dettaglio_FPD
012500081222     c                   end
012600081230      *
012700081218     c                   endif
012800081218      *
012900081230     c     parm_KEY      reade     fiFGT01l
013000081218     c                   enddo
013100081218      *
013200090102      **
013300090102      *  Non ha trovato nessuna tariffa di riferimento
013400090102     c                   if        esito = *blank and Riferimento = 'N'
013500090102     C                   move      '3'           esito
013600090102     c                   end
013700081210     C*
013800081110     C     FINE          TAG
013900081110     c                   return
014000081110     C*---------------------------------------------------------------*
014100081218      *   Copia la testata
014200081218     C*---------------------------------------------------------------*
014300081218     c     testata_FPT   begsr
014400081229      *
014500090102     c                   clear                   solo1volta        1
014600090102      *
014700081218      *  Testata tariffe
014800081218     c     FgT_KEY       setll     fiFPT01l
014900081218     c     FgT_KEY       reade     fiFPT01l
015000081218      *
015100081218     c                   dow       not %EoF(fiFPT01l)
015200081230      *  se non annullato
015300081230     c                   if        fptATB = *blank
015400081229      *
015500081229      *  e una pulizia prima della scrittura del progressivo
015600081229      *   x verificarne l'esistenza.
015700081229     c                   clear                   newPRGtes
015800090102     c                   MOVE      'S'           Riferimento       1
015900081218      *
016000081218      *  Campi chiave dell'autista specifico
016100081218     c                   clear                   fiFPTout
016200090119     c                   clear                   wfFPTout
016300081218     c                   z-add     prmPDR        tesPDR
016400081218     c                   z-add     prmCTR        tesCTR
016500090102     c                   z-add     savPRG        tesPRG
016600081218     c                   move      prmSML        tesSML
016700081218     c                   move      prmTSR        tesTSR
016800081218      *
016900081218     c                   move      fptCTD        tesCTD
017000081218     c                   move      fptTAT        tesTAT
017100081218     c                   move      fptTPG        tesTPG
017200081218     c                   move      fptTSM        tesTSM
017300081218     c                   move      fptTPP        tesTPP
017400081218     c                   z-add     fptATA        tesATA
017500081218     c                   z-add     fptARL        tesARL
017600081218     c                   z-add     fptARF        tesARF
017700081218     c                   z-add     fptARO        tesARO
017800081218     c                   z-add     fptRPV        tesRPV
017900081218     c                   move      fptFLR        tesFLR
018000081218     c                   move      fptFTR        tesFTR
018100081218     c                   z-add     fptDTR        tesDTR
018200081218      *
018300090102     c***  riscrive_T    tag
018400090102     c                   write     fiFPTout                             99
018500090119     c                   write     wfFPTout
018600090102     c   99              eval      esito = '1'
018700090102      *
018800081229      *   se si è scritto un nuovo progressivo occorre scrivere la relativa testata
018900090102      **
019000090102      **  Deve scrivere una nuova TESTATA
019100090102     C                   If        savPRG <> prmPRG    and
019200090102     c                                  solo1volta <> 'S'
019300090102      **
019400081229      ** ----------------------------------------------------------------------
019500081229      **  Occorre demandare esternamente la scrittura della testata su un altro file
019600081229      **   di work identico al FIFGT per non andare ad intralciare il ciclo di
019700081229      **   lettura delle testate di riferimento in questo programma.....poi,
019800081229      **   alla fine di tutto, il file di work verrà copiato sul FIFGT in ADD.
019900081229      **
020000081229      ** esempio: tar.  C/300 riferita all'AUT xxx
020100081229      **   tempo:
020200081229      **     Date sulla linea temporale della tariffa e di quella di riferimento
020300081229      **     >>>--------1----2----------3--4---------------5---------->>>>
020400081229      **
020500081229      **   tariffa AUT       DDT            prm                  DST
020600081229      **    da PARM          |---------------------------------- oo
020700081229      **                    (2)
020800081229      **                DDT           DST DDT             DST
020900081229      **  Tariffa di    (1)    fgt 1°  (3)(4)   fgt 2°    (5)
021000081229      **           RIFER |---------------|-----------------|
021100081229      **
021200081229      ** Le 2 testate avranno i seguenti periodi per l'autista
021300081229      **  1° periodo :  DDT = (2)    e   DST = (3)
021400081229      **  2° periodo :  DDT = (4)    e   DST = (5)
021500081229      **
021600081229      ** ----------------------------------------------------------------------
021700081229      ** Imposta i limiti della nuova testata da scrivere
021800081229      **
021900081229     c                   if        prmDDT >= fgtDDT
022000081229     c                   eval        dataDDT = prmDDT
022100081229     c                   else
022200081229     c                   eval        dataDDT = fgtDDT
022300081229     c                   end
022400081229      **
022500081230     c                   if        prmDST <= fgtDST
022600081229     c                   eval        dataDST = prmDST
022700081229     c                   else
022800081229     c                   eval        dataDST = fgtDST
022900081229     c                   end
023000081229      **
023100090102      **
023200090109     c                   call      'TNVRA12R'
023300081229     c                   parm                    prmNREC
023400090102     c                   parm                    savPRG
023500081229     c                   parm                    dataDDT
023600081229     c                   parm                    dataDST
023700090102      **
023800090102     c                   eval      solo1volta = 'S'
023900081229      **
024000081230      *  Ha scritto delle nuove testate
024100081230     C                   move      '2'           esito
024200081229     c                   end
024300081229      **
024400081218     c                   endif
024500081218      *
024600081218     c     FgT_KEY       reade     fiFPT01l
024700081218     c                   enddo
024800081230      *
024900081218     c                   endsr
025000081218     C*---------------------------------------------------------------*
025100081218      *   Copia il dettaglio
025200081218     C*---------------------------------------------------------------*
025300081218     c     dettaglio_FPD begsr
025400081218      *
025500081218      *  Dettaglio tariffe
025600081218     c     FgT_KEY       setll     fiFPD01l
025700081218     c     FgT_KEY       reade     fiFPD01l
025800081218      *
025900081218     c                   dow       not %EoF(fiFPD01l)
026000081218      *  se non annullato
026100081218     c                   if        fpdATB = *blank
026200081218      *
026300081218      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
026400081218      *    poichè non essendo gestito il progressivo sulle tariffe di
026500081218      *     riferimento occorre testare le date di decorrenza - scadenza
026600081218      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
026700081218      *
026800081218      *  Campi chiave dell'autista specifico
026900081218     c                   clear                   fiFPDout
027000090119     c                   clear                   wfFPDout
027100081218     c                   z-add     prmPDR        detPDR
027200081218     c                   z-add     prmCTR        detCTR
027300081230      *
027400090102     c                   z-add     savPRG        detPRG
027500081218     c                   move      prmSML        detSML
027600081218     c                   move      prmTSR        detTSR
027700090102      *
027800081218     c                   move      fpdCTD        detCTD
027900081218     c                   move      fpdCAP        detCAP
028000081218     c                   z-add     fpdSGL        detSGL
028100081218     c                   z-add     fpdITR        detITR
028200081218     c                   z-add     fpdMIN        detMIN
028300081218     c                   z-add     fpdMAX        detMAX
028400081218     c                   move      fpdAIN        detAIN
028500081218     c                   move      fpdFTR        detFTR
028600081218     c                   z-add     fpdDTR        detDTR
028700081218      *
028800081230     c                   write     fiFPDout
028900090119     c                   write     wfFPDout
029000081218     c                   endif
029100081218      *
029200081218     c     FgT_KEY       reade     fiFPD01l
029300081218     c                   enddo
029400081218      *
029500081218     c                   endsr
029600081218     C*---------------------------------------------------------------*
