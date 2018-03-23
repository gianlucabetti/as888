000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081121     H* TNVR11AR *---------------------------------------------------*
000300081121     H*        Duplica le Tariffe                                    *
000400000000     H*--------------------------------------------------------------*
000500081124     Ffifgt01L  iF   E           K DISK
000600081210     Ffifpt01L  iF   E           K DISK
000700081210     Ffifpt00F  o  a E           K DISK    RENAME(FIFPT000:FIFPTout)
000800081210     F                                     prefix(tes:3)
000900081210     Ffifpd01L  iF   E           K DISK
001000081210     Ffifpd00F  o  a E           K DISK    RENAME(FIFPD000:FIFPDout)
001100081210     F                                     prefix(det:3)
001200081110      *
001300081121     D FGT_DS        e ds                  EXTNAME(FIFGT00F)
001400081110      *
001500931202     D KPJBA         E DS
001600081124      *
001700081209     D Data_Oggi       s              8s 0
001800081209      *
001900081209     D prmPDR          s                   like(FgtPDR)
002000081124     D prmSML          s                   like(FgtSML)
002100081124     D prmTSR          s                   like(FgtTSR)
002200081124     D prmCTR          s                   like(FgtCTR)
002300081124     D prmPRG          s                   like(FgtPRG)
002400081124     D prmRAP          s                   like(FgtRAP)
002500081124     D prmRCT          s                   like(FgtRCT)
002600081210     D prmDDT          s                   like(FgtDDT)
002700081210     D prmDST          s                   like(FgtDST)
002800081211     D prmNREC         s              9s 0
002900900521     C*****************************************************************
003000081209      *   Crea le tariffe direttamente sull'autista fotocopiando
003100081209      *    la tariffa di riferimento.
003200081209     C*****************************************************************
003300000000     C     *ENTRY        PLIST
003400081124     C                   PARM                    prmPDR
003500081124     C                   PARM                    prmSML
003600081124     C                   PARM                    prmTSR
003700081124     C                   PARM                    prmCTR
003800081124     C                   PARM                    prmPRG
003900081124     C                   PARM                    prmRAP
004000081124     C                   PARM                    prmRCT
004100081210     c                   parm                    prmDDT
004200081210     c                   parm                    prmDST
004300081211     C                   PARM                    prmNREC
004400081121     C                   PARM                    esito             1
004500081124      *
004600081124      * Aggancia la tariffa di riferimento
004700081209     C     prm_KEY       kLIST
004800081124     C                   KFLD                    prmRAP
004900081124     C                   KFLD                    prmSML
005000081124     C                   KFLD                    prmTSR
005100081124     C                   KFLD                    prmRCT
005200081124      *
005300081209     C     fgt_KEY       kLIST
005400081209     C                   KFLD                    fgtPDR
005500081209     C                   KFLD                    fgtSML
005600081209     C                   KFLD                    fgtTSR
005700081209     C                   KFLD                    fgtCTR
005800081209     C                   KFLD                    fgtPRG
005900081209      *
006000081110     C                   SETON                                        LR
006100081110     c                   reset                   esito
006200081209     c                   move      *date         Data_Oggi
006300081121      *
006400081209     c                   clear                   trovato           1
006500081209      *
006600081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
006700081210      *    poichè non essendo gestito il progressivo sulle tariffe di
006800081210      *     riferimento occorre testare le date di decorrenza - scadenza
006900081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
007000081210      *
007100081209      *  Testata Dettagli
007200081209     c     prm_KEY       setll     fiFGT01l
007300081209     c     prm_KEY       reade     fiFGT01l
007400081209      *
007500081209     c                   dow       not %EoF(fiFGT01l)
007600081209      *
007700081210      * Tariffa di riferimento del periodo giusto
007800081210     c                   if        FGTatb = *blank
007900081210     c                   if        prmDDT >= FGTddt and prmDDT <= FGTdst or
008000081210     c                             prmDST >= FGTddt and prmDST <= FGTdst
008100081209     c                   move      'S'           trovato
008200081209     c                   leave
008300081209     c                   end
008400081210     c                   endif
008500081209      *
008600081209     c     prm_KEY       reade     fiFGT01l
008700081209     c                   enddo
008800081124      *
008900081209      * se non ha trovato il progressivo necessario x la tariffa di riferimento
009000081209      *  deve segnalare l'errore.
009100081209      *
009200081210      *  x non Trovato invia un esito di errore
009300081209     c     trovato       ifNE      'S'
009400081209     C                   move      '1'           esito
009500081209     c                   else
009600081210      * >>>>>>>>>>>>>>
009700081210      *  Testata tariffe
009800081209     c     FgT_KEY       setll     fiFPT01l
009900081209     c     FgT_KEY       reade     fiFPT01l
010000081121      *
010100081121     c                   dow       not %EoF(fiFPT01l)
010200081210      *  se non annullato
010300081210     c                   if        fptATB = *blank
010400081210      *
010500081210      *  Campi chiave dell'autista specifico
010600081210     c                   clear                   fiFPTout
010700081210     c                   z-add     prmPDR        tesPDR
010800081210     c                   z-add     prmCTR        tesCTR
010900081210     c                   z-add     prmPRG        tesPRG
011000081210     c                   move      prmSML        tesSML
011100081210     c                   move      prmTSR        tesTSR
011200081210      *
011300081210     c                   move      fptCTD        tesCTD
011400081210     c                   move      fptTAT        tesTAT
011500081210     c                   move      fptTPG        tesTPG
011600081210     c                   move      fptTSM        tesTSM
011700081210     c                   move      fptTPP        tesTPP
011800081210     c                   z-add     fptATA        tesATA
011900081210     c                   z-add     fptARL        tesARL
012000081210     c                   z-add     fptARF        tesARF
012100081210     c                   z-add     fptARO        tesARO
012200081210     c                   z-add     fptRPV        tesRPV
012300081210     c                   move      fptFLR        tesFLR
012400081210     c                   move      fptFTR        tesFTR
012500081210     c                   z-add     fptDTR        tesDTR
012600081121      *
012700081210     c                   write     fiFPTout
012800081121      *
012900081210     c                   endif
013000081210      *
013100081209     c     FgT_KEY       reade     fiFPT01l
013200081121     c                   enddo
013300081110     C*
013400081210      * >>>>>>>>>>>>>>
013500081210      *  Dettaglio tariffe
013600081210     c     FgT_KEY       setll     fiFPD01l
013700081210     c     FgT_KEY       reade     fiFPD01l
013800081210      *
013900081210     c                   dow       not %EoF(fiFPD01l)
014000081210      *  se non annullato
014100081210     c                   if        fpdATB = *blank
014200081210      *
014300081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
014400081210      *    poichè non essendo gestito il progressivo sulle tariffe di
014500081210      *     riferimento occorre testare le date di decorrenza - scadenza
014600081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
014700081210      *
014800081210      *  Campi chiave dell'autista specifico
014900081210     c                   clear                   fiFPDout
015000081210     c                   z-add     prmPDR        detPDR
015100081210     c                   z-add     prmCTR        detCTR
015200081210     c                   z-add     prmPRG        detPRG
015300081210     c                   move      prmSML        detSML
015400081210     c                   move      prmTSR        detTSR
015500081210      *
015600081210     c                   move      fpdCTD        detCTD
015700081210     c                   move      fpdCAP        detCAP
015800081210     c                   z-add     fpdSGL        detSGL
015900081210     c                   z-add     fpdITR        detITR
016000081210     c                   z-add     fpdMIN        detMIN
016100081210     c                   z-add     fpdMAX        detMAX
016200081210     c                   move      fpdAIN        detAIN
016300081210     c                   move      fpdFTR        detFTR
016400081210     c                   z-add     fpdDTR        detDTR
016500081210      *
016600081210     c                   write     fiFPDout
016700081210      *
016800081210     c                   endif
016900081210      *
017000081210     c     FgT_KEY       reade     fiFPD01l
017100081210     c                   enddo
017200081210     C*
017300081210      * >>>>>>>>>>>>>>
017400081210     c                   end
017500081210     C*
017600081110     C     FINE          TAG
017700081110     c                   return
017800081110     C*---------------------------------------------------------------*
