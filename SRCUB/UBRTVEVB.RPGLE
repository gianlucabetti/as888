000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200111216     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300991027
000400111219      * per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) UNITRAGRP
000500111219      * o la lib. dati di sede (se si è in un SI di filiale) UNITRAGRU
000600111216
000700111219      * key sped + Data evento
000800111219     FFNEVB01L  IF   E           K DISK    usropn rename(FNEVB000:FNEVB01)
000900111219     F                                     extfile(LibFileEVB01)
001000111219      * key sped + Causale
001100111219     FFNEVB04L  IF   E           K DISK    usropn rename(FNEVB000:FNEVB04)
001200111219     F                                     extfile(LibFileEVB04)
001300031103
001400041015     D*-----------
001500041015     D* VARIABILI D WRK
001600041015     D*-----------
001700111219     D LibFileEVB01    s             21A   inz
001800111219     D LibFileEVB04    s             21A   inz
001900110929     D currSysNeta     s              8A   inz('*NULL')
002000110929
002100111216
002200111216     D*-----------
002300111216     D* PARAMETRI
002400111216     D*-----------
002500111221     D pInAnno         S              4s 0
002600111221     D pInLineaPar     S              3s 0
002700111221     D pInSerie        S              2s 0
002800111221     D pInNumSped      S              7s 0
002900111221     D pInCausale      S              3a
003000111221     D pInDtEvento     S              8s 0
003100111220     D* tipo chiamata:
003200111220     D*  0=prima chiamata
003300111220     D*  1=successive
003400111220     D pInTpChiamata   S              1s 0
003500111219     D pOutFNEVB_DS    DS                  occurs(50)
003600111221     D  pOutCausale                   3a
003700111221     D  pOutDtEvento                  8s 0
003800111221     D  pOutOraEvento                 4s 0
003900111221     D  pOutDtImmEv                   8s 0
004000111221     D  pOutOraImmEv                  6s 0
004100111221     D  pOutFilImmEv                  3s 0
004200111220     D* esito reperimento:
004300111220     D*  0=non reperito
004400111220     D*  1=reperito
004500111216     D pOutEsito       S              1A
004600111219     D pOutNrOccurs    S              3S 0
004700111220     D*  0=non letti tutti i dati (da fare altra chiamata)
004800111220     D*  1=finiti di leggere tutti i dati
004900111219     D pOutEnd         S              1A
005000031103
005100110928
005200110928     D*------------------
005300111220     D* indici
005400110928     D*------------------
005500111220     D $x              s              3s 0
005600111220
005700111220
005800111220     D*------------------
005900111220     D* LINKING A DEFINIZIONI ESTERNE
006000111220     D*------------------
006100110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006200110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006300110928
006400030924
006500030325
006600030924     C* MAIN/
006700041015     C*
006800111220     C* Inizializzazione
006900111220     C                   EXSR      Inizia
007000111220     C*
007100111220     C* Tenta il reperimento
007200111220     C                   EXSR      TentaReperim
007300110929     C*
007400111216     C                   SETON                                        RT
007500111216     C                   RETURN
007600031103
007700031103
007800111220     C     Inizia        BEGSR
007900111220     C*
008000111220     C                   EVAL      pOutEsito = '0'
008100111220     C                   EVAL      pOutNrOccurs = 0
008200111220     C                   EVAL      pOutEnd = '0'
008300111216     C*
008400111220     c                   Do        50            $X
008500111220     c     $x            Occur     pOutFNEVB_DS
008600111220     C                   CLEAR                   pOutFNEVB_DS
008700111220     C                   EndDo
008800111220     C*
008900111220     C                   ENDSR
009000111220
009100111220
009200111220     C     TentaReperim  BEGSR
009300110928     C*
009400111220     C* se non passati filtri o c'è solo la data evento, uso la VL 01L
009500111220     C* se c'è solo la causale o sia causale che data, uso la VL 04L
009600111219     C*
009700111219     C                   SELECT
009800111220     C*
009900111220     C* non c'è nessun filtro
010000111220     C                   WHEN      pInDtEvento = 0 and pInCausale = *blank
010100111219     C                   if        not %open(FNEVB01L)
010200111219     C                   open      FNEVB01L
010300111216     C                   endif
010400111220     C                   IF        pInTpChiamata = 0
010500111220     C     K04EVB01      SETLL     FNEVB01
010600111220     C                   ENDIF
010700111220     C     K04EVB01      READE     FNEVB01
010800111220     C                   DOW       not %eof(FNEVB01L)
010900111220     C                   EVAL      pOutNrOccurs = pOutNrOccurs + 1
011000111220     c     pOutNrOccurs  Occur     pOutFNEVB_DS
011100111220     C                   EVAL      pOutCausale   = EVBCEV
011200111220     C                   EVAL      pOutDtEvento  = EVBDEV
011300111220     C                   EVAL      pOutOraEvento = EVBHEV
011400111220     C                   EVAL      pOutDtImmEv   = EVBDTV
011500111220     C                   EVAL      pOutOraImmEv  = EVBORV
011600111220     C                   EVAL      pOutFilImmEv  = EVBFLE
011700111220     C                   EVAL      pOutEsito = '1'
011800111220     C* se non ho raggiunto il limite dell'occortrenza leggo un altro rcd
011900111220     C                   IF        pOutNrOccurs < 50
012000111220     C     K04EVB01      READE     FNEVB01
012100111220     C* altrimenti esco dal ciclo
012200111220     C                   ELSE
012300111220     C                   LEAVE
012400111220     C                   ENDIF
012500111220     C                   ENDDO
012600111220     C                   IF        %eof(FNEVB01L)
012700111220     C                   EVAL      pOutEnd = '1'
012800111220     C                   ENDIF
012900111220     C*
013000111220     C* c'è solo la data evento
013100111220     C                   WHEN      pInCausale = *blank
013200111220     C                   if        not %open(FNEVB01L)
013300111220     C                   open      FNEVB01L
013400111220     C                   endif
013500111220     C                   IF        pInTpChiamata = 0
013600111220     C     K05EVB01      SETLL     FNEVB01
013700111220     C                   ENDIF
013800111220     C     K05EVB01      READE     FNEVB01
013900111220     C                   DOW       not %eof(FNEVB01L)
014000111220     C                   EVAL      pOutNrOccurs = pOutNrOccurs + 1
014100111220     c     pOutNrOccurs  Occur     pOutFNEVB_DS
014200111220     C                   EVAL      pOutCausale   = EVBCEV
014300111220     C                   EVAL      pOutDtEvento  = EVBDEV
014400111220     C                   EVAL      pOutOraEvento = EVBHEV
014500111220     C                   EVAL      pOutDtImmEv   = EVBDTV
014600111220     C                   EVAL      pOutOraImmEv  = EVBORV
014700111220     C                   EVAL      pOutFilImmEv  = EVBFLE
014800111220     C                   EVAL      pOutEsito = '1'
014900111220     C* se non ho raggiunto il limite dell'occortrenza leggo un altro rcd
015000111220     C                   IF        pOutNrOccurs < 50
015100111220     C     K05EVB01      READE     FNEVB01
015200111220     C* altrimenti esco dal ciclo
015300111220     C                   ELSE
015400111220     C                   LEAVE
015500111220     C                   ENDIF
015600111220     C                   ENDDO
015700111220     C                   IF        %eof(FNEVB01L)
015800111220     C                   EVAL      pOutEnd = '1'
015900111220     C                   ENDIF
016000111220     C*
016100111220     C* c'è solo la causale
016200111220     C                   WHEN      pInDtEvento =  0
016300111220     C                   if        not %open(FNEVB04L)
016400111220     C                   open      FNEVB04L
016500111220     C                   endif
016600111220     C                   IF        pInTpChiamata = 0
016700111220     C     K05EVB04      SETLL     FNEVB04
016800111220     C                   ENDIF
016900111220     C     K05EVB04      READE     FNEVB04
017000111220     C                   DOW       not %eof(FNEVB04L)
017100111220     C                   EVAL      pOutNrOccurs = pOutNrOccurs + 1
017200111220     c     pOutNrOccurs  Occur     pOutFNEVB_DS
017300111220     C                   EVAL      pOutCausale   = EVBCEV
017400111220     C                   EVAL      pOutDtEvento  = EVBDEV
017500111220     C                   EVAL      pOutOraEvento = EVBHEV
017600111220     C                   EVAL      pOutDtImmEv   = EVBDTV
017700111220     C                   EVAL      pOutOraImmEv  = EVBORV
017800111220     C                   EVAL      pOutFilImmEv  = EVBFLE
017900111220     C                   EVAL      pOutEsito = '1'
018000111220     C* se non ho raggiunto il limite dell'occortrenza leggo un altro rcd
018100111220     C                   IF        pOutNrOccurs < 50
018200111220     C     K05EVB04      READE     FNEVB04
018300111220     C* altrimenti esco dal ciclo
018400111220     C                   ELSE
018500111220     C                   LEAVE
018600111220     C                   ENDIF
018700111220     C                   ENDDO
018800111220     C                   IF        %eof(FNEVB04L)
018900111220     C                   EVAL      pOutEnd = '1'
019000111220     C                   ENDIF
019100111220     C*
019200111220     C* ci sono tutti i filtri
019300111220     C                   OTHER
019400111220     C                   if        not %open(FNEVB04L)
019500111220     C                   open      FNEVB04L
019600111220     C                   endif
019700111220     C                   IF        pInTpChiamata = 0
019800111220     C     K06EVB04      SETLL     FNEVB04
019900111220     C                   ENDIF
020000111220     C     K06EVB04      READE     FNEVB04
020100111220     C                   DOW       not %eof(FNEVB04L)
020200111220     C                             and pOutNrOccurs < 50
020300111220     C                   EVAL      pOutNrOccurs = pOutNrOccurs + 1
020400111220     c     pOutNrOccurs  Occur     pOutFNEVB_DS
020500111220     C                   EVAL      pOutCausale   = EVBCEV
020600111220     C                   EVAL      pOutDtEvento  = EVBDEV
020700111220     C                   EVAL      pOutOraEvento = EVBHEV
020800111220     C                   EVAL      pOutDtImmEv   = EVBDTV
020900111220     C                   EVAL      pOutOraImmEv  = EVBORV
021000111220     C                   EVAL      pOutFilImmEv  = EVBFLE
021100111220     C                   EVAL      pOutEsito = '1'
021200111220     C* se non ho raggiunto il limite dell'occortrenza leggo un altro rcd
021300111220     C                   IF        pOutNrOccurs < 50
021400111220     C     K06EVB04      READE     FNEVB04
021500111220     C* altrimenti esco dal ciclo
021600111220     C                   ELSE
021700111220     C                   LEAVE
021800111220     C                   ENDIF
021900111220     C                   ENDDO
022000111220     C                   IF        %eof(FNEVB04L)
022100111220     C                   EVAL      pOutEnd = '1'
022200111220     C                   ENDIF
022300110928     C*
022400111220     C                   ENDSL
022500031103     C*
022600031103     C                   ENDSR
022700031103
022800031103
022900991027
023000991027      /TITLE Operazioni iniziali.
023100991027     C     *inzsr        BEGSR
023200991027     C*
023300991027     C     *ENTRY        PLIST
023400111216     C                   PARM                    pInAnno
023500111216     C                   PARM                    pInLineaPar
023600111216     C                   PARM                    pInSerie
023700111216     C                   PARM                    pInNumSped
023800111220     C                   PARM                    pInCausale
023900111220     C                   PARM                    pInDtEvento
024000111220     C                   PARM                    pInTpChiamata
024100111220     C                   PARM                    pOutFNEVB_DS
024200111216     C                   PARM                    pOutEsito
024300111220     C                   PARM                    pOutNrOccurs
024400111220     C                   PARM                    pOutEnd
024500111216     C*
024600111216     C* Reperisco il sistema AS/400 corrente
024700111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
024800111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
024900111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
025000111220     C                   eval      LibFileEVB01 = 'UNITRAGRU/FNEVB01L'
025100111220     C                   eval      LibFileEVB04 = 'UNITRAGRU/FNEVB04L'
025200111216     C                   else
025300111220     C                   eval      LibFileEVB01 = 'UNITRAGRP/FNEVB01L'
025400111220     C                   eval      LibFileEVB04 = 'UNITRAGRP/FNEVB04L'
025500111216     C                   endif
025600030709     C*
025700030709     C* Definizione chiavi
025800111220     C*
025900111220     C* Chiave su FNEVB01L parziale
026000111220     C     K04EVB01      KLIST
026100111216     C                   KFLD                    pInAnno
026200111216     C                   KFLD                    pInLineaPar
026300111216     C                   KFLD                    pInSerie
026400111216     C                   KFLD                    pInNumSped
026500111220     C     K05EVB01      KLIST
026600111220     C                   KFLD                    pInAnno
026700111220     C                   KFLD                    pInLineaPar
026800111220     C                   KFLD                    pInSerie
026900111220     C                   KFLD                    pInNumSped
027000111220     C                   KFLD                    pInDtEvento
027100111220     C*
027200111220     C* Chiave su FNEVB04L parziale
027300111220     C     K05EVB04      KLIST
027400111220     C                   KFLD                    pInAnno
027500111220     C                   KFLD                    pInLineaPar
027600111220     C                   KFLD                    pInSerie
027700111220     C                   KFLD                    pInNumSped
027800111220     C                   KFLD                    pInCausale
027900111220     C     K06EVB04      KLIST
028000111220     C                   KFLD                    pInAnno
028100111220     C                   KFLD                    pInLineaPar
028200111220     C                   KFLD                    pInSerie
028300111220     C                   KFLD                    pInNumSped
028400111220     C                   KFLD                    pInCausale
028500111220     C                   KFLD                    pInDtEvento
028600991027     C*
028700991027     C                   ENDSR
