000100150528      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400180306     Ftivin00r  uF   E             DISK    usropn
000500150528     FEDIVATwr  O    E             DISK    usropn
000600000313     D*
000700040726     D*------------
000800040726     D* SCHIEDE A PROGRAMMA
000900040726     D*------------
001000040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002400180202
002500180202       // - Parametri ricevuti:
002600180202     d i_Opz           s              1a
002700180202       // tipo lancio: 'C'=LR   *blank=RT
002800180202     d i_TLa           s              1a
002900180202     d old_AAS         s              4s 0
003000180202     d old_LNP         s              3s 0
003100180202     d old_NRS         s              2s 0
003200180202     d old_NSP         s              7s 0
003300180202     d old_PKB         s              7s 1
003400180306     d old_NCL         s              5s 0
003500180202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
003600180202     d o_Esito         s              1s 0
003700180202
003800030715     D*------------------
003900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000030715     D*------------------
004100030715     D SkSplitFLD      S             10    DIM(100)
004200030715     D SkSplitCSV      S            256    DIM(100)
004300030715     D CharCSV         S              1
004400030715     D CharTXT         S              1
004500030715     D CharNUM         S              1
004600030715     D posDa           S              3  0 INZ(*zeros)
004700030715     D posA            S              3  0 INZ(*zeros)
004800030715     D i               s              3  0 INZ(1)
004900030715     D wGiro           s              1  0 INZ(*zeros)
005000130625     D w50             s             50
005100130625     D wX              s              3  0
005200130625     D num             c                   const('1234567890 ')
005300130528     D wVATNOT_E       s                   LIKE(VATNOT)
005400180206     D wVATPKB         s              7s 1
005500030822     D*------------------
005600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005700030822     D*------------------
005800030822     D posDaDft        S              3  0 INZ(*zeros)
005900030822     D posADft         S              3  0 INZ(*zeros)
006000030822     D j               s              3  0 INZ(1)
006100030822     D wGiroDft        s              1  0 INZ(*zeros)
006200010330     D*------------------
006300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400010330     D*------------------
006500010330     D WLBDA8          DS                  INZ
006600010330     D  G08DAT                 1      8  0
006700010330     D  G08INV                 9     16  0
006800010330     D  G08ERR                17     17
006900010330     D  G08TGI                18     22  0
007000110222
007100031201     D*------------------
007200031201     D* Costanti
007300031201     D*------------------
007400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600010330
007700010330
007800010330
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010601     C*
008300010601     C                   exsr      opeini
008400040714     C                   exsr      rwvat
008500010601     C*
008600010601     C                   seton                                        lr
008700010601
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009300010601     C*--------------------------------------------------------
009400010601     C     PREELA        BEGSR
009500010601     C*
009600010601     C* SE OCCORRE SPEDIRE IN FILIALE
009700010601     C                   if        invfil <> *zeros and
009800010601     C                             flgGiro = '0'
009900010601     C*
010000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010100010601     C                   eval      flgGiro = '1'
010200010601     C*
010300010601     C                   endif
010400010601     C*
010500010601     C                   ENDSR
010600010601     C***
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800000616     C*
011900010601     C                   ENDSR
012000010601     C***
012100000613
012200010330
012300010601
012400010601
012500010601
012600010330     C*--------------------------------------------------------
012700150528     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
012800010330     C*--------------------------------------------------------
012900040714     C     RWVAT         BEGSR
013000010330     C*
013100010330     C                   if        not %open(tivin00r)
013200010330     C                   open      tivin00r
013300010330     C                   endif
013400150528     C                   if        not %open(EDIVATwr)
013500150528     C                   open      EDIVATwr
013600010330     C                   endif
013700010330     C*
013800010604     C                   clear                   §CTROK
013900010604     C                   clear                   §CTRMO
014000010604     C                   clear                   §CTRNO
014100040726     C*
014200040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014300040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014400040726     C                   exsr      inzvar
014500040726     C                   exsr      impvat
014600010330     C*
0147001802071    C                   do        *HIVAL
014800010330     C*
014900030822     C                   read      tivin00r                               70
015000010618     C*
015100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015200010618     C                   if        vinflg = '1'
015300010618     C                   eval      flgOk = '1'
015400010618     C                   endif
015500010618     C*
015600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015700040510     C                   z-add     *zeros        wLenVINDTA        4 0
015800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016200040510     C                   z-add     1             wFlgVINDTA
016300040510     C                   leave
016400040510     C                   endif
016500040510     C                   enddo
016600040510     C*
0167001802072    C                   if        vindta > *blanks AND
016800040510     C                             wFlgVINDTA = 1
016900010330     C                   add       1             rrnum
017000010330     C*
0171001802073    C                   if        *in70 = *off and
017200010330     C                             (vinflg = *blanks
017300010330     C                              or vinflg = '0'
017400010330     C                              or vinflg = '2')
017500010330     C*
017600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017700010711     C                   if        vinflg = *blanks or vinflg = '0'
017800010711     C                   clear                   vinmsg
017900010711     C                   endif
018000010601     C*
018100010330     C                   exsr      inzvar
018200040714     C                   exsr      impvat
018300010601     C*
018400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018500040714     C                   eval      depfil = VATLNP
018600010601     C                   exsr      repfil
0187001802074    C                   if        depfil = invfil
018800180207     C                   if        vlrpoi = 999
018900040714     C                   move(P)   invfil        VATFGS
019000021025     C                   else
019100040714     C                   move(P)   vlrpoi        VATFGS
019200021025     C                   endif
019300010601     C*
019400010601     C                   exsr      PREELA
019500010601     C*
019600010604     C* Ebbene...
019700010604     C*
019800030822     C  N31              add       1             §CTROK            7 0
019900030822     C   32              add       1             §CTRMO            7 0
020000030822     C   31              add       1             §CTRNO            7 0
0201001802075    C                   if        wGiro = 2
020200110222     C*
0203001802076    C                   if        not *in30
020400110222     C*
020500130528     C* inserimento tipo record 'E'
0206001802077    C                   if        wVATNOT_E <> *blanks
020700130528     C                   eval      VATNOT = wVATNOT_E
020800130528     C                   eval      VATTRC = 'E'
020900150529     C                   eval      VATCMR = 'GAO'
021000150528     C                   eval      VATCNT = 1
021100180202     C*
0212001802078    C                   if        *in31 = *off
021300180202     C*
021400180202     C* se è la prima volta, imposto i dati di controllo
021500180202     C                   if        old_AAS = 0 and
021600180202     C                             old_LNP = 0 and
021700180202     C                             old_NRS = 0 and
021800180202     C                             old_NSP = 0
021900180202     C                   eval      old_AAS = VATAAS
022000180202     C                   eval      old_LNP = VATLNP
022100180202     C                   eval      old_NRS = VATNRS
022200180202     C                   eval      old_NSP = VATNSP
022300180202     C                   endif
022400180207
022500180306     C* controllo se il rcd che scriverò ha una chiave diversa da quella per cui sto valorizzando
022600180306     C* peso e numero colli
022700180306     C* se sì devo aggiornare su VAB il peso e il numero colli della spedizione precedente
0228001802079    C                   if        VATAAS <> old_AAS or
022900180202     C                             VATLNP <> old_LNP or
023000180202     C                             VATNRS <> old_NRS or
023100180202     C                             VATNSP <> old_NSP
023200180306     C                   call      'TITZ90R31'
023300180202     C                   parm                    i_OPZ
023400180202     C                   parm      *blank        i_TLA
023500180202     C                   parm                    old_AAS
023600180202     C                   parm                    old_LNP
023700180202     C                   parm                    old_NRS
023800180202     C                   parm                    old_NSP
023900180202     C                   parm                    old_PKB
024000180306     C                   parm                    old_NCL
024100180202     C                   parm                    o_Esito
024200180206     C*
024300180206     C* adesso è la nuova spedizione da tenere in considerazione
024400180206     C                   eval      old_AAS = VATAAS
024500180206     C                   eval      old_LNP = VATLNP
024600180207
024700180206     C                   eval      old_NSP = VATNSP
024800180206     C                   eval      old_PKB = wVATPKB
024900180306     C                   eval      old_NCL = 1
025000180202     C*
025100180205     C* se no devo sommare il peso a quello della riga precedente
025200180306     C*  e devo aumentare il valore del numero colli
025300180202     C                   else
025400180206     C                   eval      old_PKB = old_PKB + wVATPKB
025500180306     C                   eval      old_NCL += 1
0256001802079-   C                   endif
025700180205     C*
025800180202     C                   write     EDIVAT00
025900180202     C**N31              write     EDIVAT00
0260001802078-   C                   endif
026100180202     C*
0262001802077-   C                   endif
0263001802076-   C                   endif
0264001802075-   C                   endif
026500010604     C*
0266001802075    C                   if        *in31 = *off and
026700150528     C                             *in32 = *off
026800010604     C                   eval      vinflg = '1'
026900010604     C                   else
027000010604     C                   eval      vinflg = '2'
0271001802075-   C                   endif
0272001802074-   C                   endif
027300010601     C*
0274001802073-   C                   endif
027500010604     C*
0276001802072e   C                   else
027700010330     C                   eval      vinflg = '1'
0278001802072-   C                   endif
027900010601     C*
028000180207     C* se non ci sono più rcd su TIVIN00R
028100180207     C                   if        *in70
028200180207     C* ma ho una spedizione da aggiornare (ho letto almeno un rcd di TIVIN00R)
028300180207     C                             and old_NSP > 0
028400180207     C* scarico il buffer
028500180306     C                   call      'TITZ90R31'
028600180207     C                   parm                    i_OPZ
028700180207     C                   parm      *blank        i_TLA
028800180207     C                   parm                    old_AAS
028900180207     C                   parm                    old_LNP
029000180207     C                   parm                    old_NRS
029100180207     C                   parm                    old_NSP
029200180207     C                   parm                    old_PKB
029300180306     C                   parm                    old_NCL
029400180207     C                   parm                    o_Esito
029500180207     C                   endif
029600180207     C*
029700010601     C  N70              update    tivin000
029800010330     C*
0299001802071-   C  N70              enddo
030000010601     C
030100010601     C*
030200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030300010601     C                   if        cntNonEl = *zeros or
030400010601     C                             flgMulti = '0'
030500010330     C* Se non ci sono record con errori ...
030600010601     C                   if        §ctrno = 0 and
030700010604     C                             §ctrmo = 0 and
030800010601     C                             flgStato <> '2'
030900010330     C* ... restituisco esito OK.
031000010330     C                   eval      wrkesito = '0'
031100010330     C                   else
031200010330     C                   if        §ctrok > 0
031300010330     C                   eval      wrkesito = '1'
031400010330     C                   else
031500010615     C                   if        flgOk = '0'
031600010615     C                   eval      wrkesito = '2'
031700010615     C                   else
031800010615     C                   eval      wrkesito = '6'
031900010615     C                   endif
032000010330     C                   endif
032100010330     C                   endif
032200010601     C                   else
032300010601     C                   eval      wrkesito = '9'
032400010601     C                   endif
032500010330     C*
032600010330     C                   if        %open(tivin00r)
032700010330     C                   close     tivin00r
032800010330     C                   endif
032900150528     C                   if        %open(EDIVATwr)
033000150528     C                   close     EDIVATwr
033100010330     C                   endif
033200010601     C*
033300010601     C                   if        vlrpoi <> 999
033400010601     C                   eval      invfil = vlrpoi
033500010601     C                   endif
033600010330     C*
033700010330     C                   if        §ctrok > 0
033800010601     C                             and invfil > *zeros
033900010330     C                   exsr      invio
034000010330     C                   endif
034100010601     C*
034200010618     C                   if        flgGiro = '1'
034300010601     C                   exsr      endela
034400010618     C                   endif
034500010330     C*
034600010330     C                   ENDSR
034700010330     C***
034800010601
034900010601
035000010601
035100010330     C*----------------------------------------------------*
035200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035300010330     C*----------------------------------------------------*
035400010330     C     INZVAR        BEGSR
035500010330     C*
035600030822     C* Inizializzo variabili di wrk
035700020204     C                   Z-ADD     *zeros        Num5_0            5 0
035800030822     C*
035900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036000150528     C                   CLEAR                   EDIVAT00
036100030822     C                   CLEAR                   SkSplitCSV
036200030822     C                   EVAL      i = 1
036300030822     C                   EVAL      posDa = *zeros
036400030822     C                   EVAL      posA  = *zeros
036500110222     C*
036600130528     C                   MOVEL     *blanks       wVATNOT_E
036700180206     C                   CLEAR                   wVATPKB
036800030822     C*
036900030822     C* Reimposto i valori di default
037000030822     C                   EXSR      DEFCAM
037100030822     C*
037200010330     C                   ENDSR
037300010330     C*----------------------------------------------------*
037400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037500010330     C*----------------------------------------------------*
037600010330     C     DEFCAM        BEGSR
037700030822     C*
037800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037900030715     C* e delimitatore testo.
038000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038300030822     C*
038400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038600030715     C                   SETON                                        50
038700030715     C                   ELSE
038800030715     C                   SETOFF                                       50
038900030715     C                   ENDIF
039000030822     C*
039100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039200030822     C                   EVAL      posDaDft = 1
039300030822     C                   EVAL      posADft  = 0
039400030822     C                   EVAL      wGiroDft = 0
039500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039600030822     C                             posDaDft > 0
039700030822     C*
039800030822     C* Gestisco il 1° giro
039900030822     C                   IF        wGiroDft = 0
040000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040200030822     C* Incremento il contatore dei "giri"
040300030822     C                   EVAL      wGiroDft = 1
040400030822     C                   ELSE
040500030822     C                   EVAL      posDaDft = posADft
040600030822     C                   ENDIF
040700030822     C* Eseguo lo scan x trovare la fine del campo corrente
040800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040900030822     C*
041000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041200030822     C                   IF        posDaDft > 0 AND
041300030822     C                             posADft  > 0
041400030822     C* CCM
041500030822     C                   IF        %subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):1:3)
041800030822     C                             = 'CCM'
041900030822     C                   EVAL      PiStr=%trim(%subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200030822     C                   EXSR      CHKNUM
042300030822     C                   IF        PiInt=*on
042400040714     C                   Z-ADD     PiVal         VATCCM
042500030822     C                   ENDIF
042600030822     C                   ENDIF
042700030822     C* LNP
042800030822     C                   IF        %subst(
042900030822     C                             %subst(vlrppt:posDaDft+1:
043000030822     C                             posADft-posDaDft-1):1:3)
043100030822     C                             = 'LNP'
043200030822     C                   EVAL      PiStr=%trim(%subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500030822     C                   EXSR      CHKNUM
043600030822     C                   IF        PiInt=*on
043700040714     C                   Z-ADD     PiVal         VATLNP
043800030822     C                   ENDIF
043900030822     C                   ENDIF
044000040714     C* TRC
044100030822     C                   IF        %subst(
044200030822     C                             %subst(vlrppt:posDaDft+1:
044300030822     C                             posADft-posDaDft-1):1:3)
044400040714     C                             = 'TRC'
044500040714     C                   EVAL      VATTRC=%trim(%subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800030822     C                   ENDIF
044900030822     C* ...
045000030822     C                   ENDIF
045100030822     C                   ENDDO
045200020204     C*
045300010330     C                   ENDSR
045400010607     C*----------------------------------------------------*
045500040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
045600010607     C*----------------------------------------------------*
045700040714     C     IMPVAT        BEGSR
045800010607     C*
045900110222     C                   SETOFF                                       313230
046000030515     C*
046100040714     C                   MOVEL     datcor        VATAAS
046200010607     C*
046300030715     C**********
046400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
046500030715     C**********
046600030715     C                   IF        wGiro = *zeros
046700040726     C*
046800040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
046900040726     C                   EVAL      depvinDTA = vinDTA
047000040726     C*
047100040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
047200040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
047300040726     C                                      %trim(intcol(3))
047400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
047500031201     C     minu:maiu     XLATE     vindta        vindta
047600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047700040324     C                   DOW       posDa <= %len(%trimr(vindta))
047800030715     C*
047900030715     C* Gestisco il 1° campo
048000030715     C                   IF        i = 1
048100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
048200030715     C                   EVAL      posDa = 1
048300030715     C* Eseguo lo scan x trovare la fine del primo campo
048400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048500030715     C* A questo punto "estraggo" il campo corrente
048600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048700030715     C                                                    (posA-posDa))
048800030715     C* X i campi successivi al 1°
048900030715     C                   ELSE
049000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
049100030715     C                   EVAL      posDa = posA + 1
049200030715     C* Eseguo lo scan x trovare la fine del campo corrente
049300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049400030715     C* Gestisco l'ultimo campo
049500030715     C                   IF        posA = *zeros
049600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
049700030715     C* A questo punto "estraggo" il campo corrente
049800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049900030715     C                                                    (posA-posDa))
050000030715     C                   LEAVE
050100030715     C                   ELSE
050200030715     C* A questo punto "estraggo" il campo corrente
050300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
050400030715     C                                                    (posA-posDa))
050500030715     C                   ENDIF
050600030715     C                   ENDIF
050700030715     C* Incremento il contatore d campo
050800030715     C                   EVAL      i = i +1
050900030715     C                   ENDDO
051000030715     C*
051100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
051200030715     C* delimitazione testo
051300030715     C                   EVAL      i = 1
051400030715     C                   DOW       i < %elem(SkSplitFLD)
051500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
051600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
051700030715     C                   EVAL      i = i + 1
051800030715     C                   ENDDO
051900030715     C                   EVAL      wGiro = 1
052000040726     C* Ripristino la 1° riga originale
052100040726     C                   EVAL      vinDTA = depvinDTA
052200030715     C                   ELSE
052300030715     C                   EVAL      wGiro = 2
052400031204     C**********
052500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
052600031204     C* d separatore campo
052700031204     C**********
052800040726     C                   EVAL      vindta = %trim(vindta)
052900040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
053000040726     C                   EVAL      vindta = %subst(vindta:2)
053100040726     C                   ENDDO
053200031204     C*
053300031204     C                   Z-ADD     *zeros        lunghInput        4 0
053400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
053500040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
053600040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
053700040726     C                   EVAL      lunghInput = %len(%trim(vindta))
053800040726     C                   ENDDO
053900030715     C**********
054000030715     C* Effettuo lo split del campo dati d input
054100030715     C**********
054200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
054300040324     C                   DOW       posDa <= %len(%trimr(vindta))
054400030715     C*
054500030715     C* Gestisco il 1° campo
054600030715     C                   IF        i = 1
054700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
054800030715     C                   EVAL      posDa = 1
054900030715     C* Eseguo lo scan x trovare la fine del primo campo
055000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055100030715     C* A questo punto "estraggo" il campo corrente
055200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055300030715     C                                                    (posA-posDa))
055400030715     C* X i campi successivi al 1°
055500030715     C                   ELSE
055600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
055700030715     C                   EVAL      posDa = posA + 1
055800031204     C* Verifico che nn vi sia il campo nn valorizzato
055900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
056000031204     C* Se campo nn valorizzato skippo al prossimo
056100031204     C                   EVAL      SkSplitCSV(i) = *blanks
056200031204     C                   EVAL      posA  = posA + 1
056300031204     C                   ELSE
056400030715     C* Eseguo lo scan x trovare la fine del campo corrente
056500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
056600030715     C* Gestisco l'ultimo campo
056700030715     C                   IF        posA = *zeros
056800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
056900030715     C* A questo punto "estraggo" il campo corrente
057000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
057100030715     C                                                    (posA-posDa))
057200030715     C                   LEAVE
057300030715     C                   ELSE
057400030715     C* A questo punto "estraggo" il campo corrente
057500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
057600030715     C                                                    (posA-posDa))
057700030715     C                   ENDIF
057800031204     C                   ENDIF
057900030715     C                   ENDIF
058000030715     C* Incremento il contatore d campo
058100030715     C                   EVAL      i = i +1
058200030715     C                   ENDDO
058300030715     C*
058400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
058500030715     C* delimitazione testo
058600030715     C                   EVAL      i = 1
058700030715     C                   DOW       i < %elem(SkSplitCSV)
058800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
058900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
059000030715     C                   EVAL      i = i + 1
059100030715     C                   ENDDO
059200030715     C*
059300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
059400030715     C                   EVAL      i = 1
059500030715     C                   DOW       i < %elem(SkSplitFLD)
059600030715     C*
059700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
059900030715     C                             SkSplitCSV(i) <> *blanks
060000030721     C*** ATB
060100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
060200040714     C                   EVAL      VATATB = SkSplitCSV(i)
060300030721     C                   ENDIF
060400040714     C*** TRC
060500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
060600040714     C                   EVAL      VATTRC = SkSplitCSV(i)
060700030715     C                   ENDIF
060800040714     C*** NOT
060900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
061000040714     C                   EVAL      VATNOT = SkSplitCSV(i)
061100030721     C                   ENDIF
061200030721     C*
061300030721     C* Reperisco quindi i campi numerici...
061400030721     C*** NSP
061500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
061600030721     C                   EVAL      PiStr=SkSplitCSV(i)
061700030721     C                   EXSR      CHKNUM
061800040726     C                   IF        PiInt=*on
061900040726     C                   Z-ADD     PiVal         VATNSP
062000040802     C*                  IF        PiVal>9999999
062100040802     C*                  SETON                                        32
062200040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
062300040802     C*                            + ' ' + 'VATNSP'
062400040802     C*                  ENDIF
062500030721     C                   ELSE
062600071002     C                   SETON                                        32
062700040714     C                   EVAL      VATNSP = *zeros
062800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
062900040714     C                             + ' ' + 'VATNSP'
063000030721     C                   ENDIF
063100030721     C                   ENDIF
063200030721     C*** CCM
063300040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
063400030721     C                   EVAL      PiStr=SkSplitCSV(i)
063500030721     C                   EXSR      CHKNUM
063600030721     C                   IF        PiInt=*on      AND
063700030721     C                             PiVal<=9999999 AND
063800030721     C                             PiVal>*zeros
063900040714     C                   Z-ADD     PiVal         VATCCM
064000030721     C                   ELSE
064100030721     C                   SETON                                        32
064200040714     C                   EVAL      VATCCM = *zeros
064300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
064400040714     C                             + ' ' + 'VATCCM'
064500030721     C                   ENDIF
064600030721     C                   ENDIF
064700030721     C*** LNP
064800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
064900030721     C                   EVAL      PiStr=SkSplitCSV(i)
065000030721     C                   EXSR      CHKNUM
065100030721     C                   IF        PiInt=*on    AND
065200030721     C                             PiVal<=999   AND
065300030721     C                             PiVal>*zeros
065400040714     C                   Z-ADD     PiVal         VATLNP
065500030721     C                   ELSE
065600030721     C                   SETON                                        32
065700040714     C                   EVAL      VATLNP = *zeros
065800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
065900040714     C                             + ' ' + 'VATLNP'
066000030721     C                   ENDIF
066100030721     C                   ENDIF
066200030721     C*** NRS
066300040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
066400030721     C                   EVAL      PiStr=SkSplitCSV(i)
066500030721     C                   EXSR      CHKNUM
066600030721     C                   IF        PiInt=*on AND
066700030721     C                             PiVal<=99
066800040714     C                   Z-ADD     PiVal         VATNRS
066900030721     C                   ELSE
067000030721     C                   SETON                                        32
067100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
067200040714     C                             + ' ' + 'VATNRS'
067300030721     C                   ENDIF
067400030721     C                   ENDIF
067500130528     C*** VATNOT_E
067600130528     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
067700130528     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
067800130528     C                   ENDIF
067900110222     C*
068000110222     C***  ===>  Gestione campi "particolari" dentro ciclo
068100130625      *** W_VATNSP: è sia VABRMA che VABNSP ma solo gli ultimi char a dx numerici
068200130625     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNSP'
068300130625     C                   EVAL      w50 = %trim(SkSplitCSV(i))
068400130625     C* trovo l'ultimo carattere non numerico e non blank
068500130625     C                   EVAL      wX = %checkR(num : W50) + 1
068600130625     C                   EVAL      VATNSP = %dec(
068700130625     C                                   %subst(w50:wX:
068800130625     C                                    %scan(' ':w50)-wX)
068900130625     C                                                 :7:0)
069000130528     C                   ENDIF
069100180206     C*
069200180206     C*** PKB
069300180206     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATPKB'
069400180206     C                   EVAL      PiStr=SkSplitCSV(i)
069500180206     C                   EXSR      CHKNUM
069600180206     C                   IF        PiNum=*on
069700180206     C                   EVAL(H)   wVATPKB = PiVal
069800180206     C                   ELSE
069900180206     C                   EVAL      wVATPKB = *zeros
070000180206     C                   ENDIF
070100180206     C                   ENDIF
070200110222     C*
070300110222     C***  <===  -----------------------------------------
070400030715     C*
070500030715     C                   ENDIF
070600030715     C                   EVAL      i = i + 1
070700030715     C                   ENDDO
070800180206     C*
070900180206     C***  ===>  Gestione campi "particolari" fuori ciclo
071000040726     C*
071100180206     C*
071200180206     C***  <===  -----------------------------------------
071300180206     C*
071400040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
071500040726     C                   IF        VATNSP = *zeros
071600040726     C                   SETON                                        31
071700040726     C                   ENDIF
071800040726     C*
071900040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
072000040726     C                   IF        VATLNP = *zeros
072100040726     C                   SETON                                        31
072200040726     C                   ENDIF
072300020204     C*
072400030715     C                   ENDIF
072500020204     C*
072600010607     C                   ENDSR
072700010607     C*----------------------------------------------------*
072800040714
072900010330
073000010330
073100010330     C*----------------------------------------------------*
073200010330     C*  CONTROLLO NUMERICITA' CAMPI
073300010330     C*----------------------------------------------------*
073400010330     C     CHKNUM        BEGSR
073500010330     C*
073600010606     C                   IF        PiDecChr = *blanks
073700030715     C                   EVAL      PiDecChr = CharNUM
073800010606     C                   ENDIF
073900010606     C*
074000010606     C                   CALL(e)   'ISNUMERIC'
074100010330     C                   PARM                    PiStr            30
074200010606     C                   PARM                    PiDecChr          1
074300010330     C                   PARM      *ZEROS        PiVal            30 9
074400010330     C                   PARM      '0'           PiInt             1
074500010330     C                   PARM      '0'           PiNum             1
074600010330     C                   IF        %error
074700010606     C                   EVAL      PiNum=*off
074800010330     C                   ENDIF
074900010330     C*
075000010330     C                   ENDSR
075100010330     C***
075200010330
075300010601
075400010601
075500010601
075600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
075700010601     C     repfil        BEGSR
075800010601     C*
075900010601     C                   if        invfil = *zeros and
076000010601     C                             depfil > *zeros and
076100010629     C                             (vinflg = *blanks or
076200010629     C                              vinflg = *zeros)
076300010601     C
076400010601     C                   eval      invfil = depfil
076500010601     C                   endif
076600010601     C*
076700010601     C                   if        depfil <> invfil and
076800010601     C                             invfil > *zeros
076900010601     C                   eval      flgMulti = '1'
077000010601     C                   if        vinflg = *blanks
077100010601     C                   add       1             cntNonEl
077200010601     C                   endif
077300010601     C                   endif
077400010601     C*
077500010601     C                   if        vinflg = '2'
077600010601     C                   eval      flgStato = '2'
077700010601     C                   endif
077800010601     C*
077900010601     C                   ENDSR
078000010601     C***
078100010601
078200010601
078300010601
078400010330
078500010330
078600010330
078700990920      /TITLE Invio dei dati al punto operativo.
078800000613     C     invio         BEGSR
078900990920     C*
079000990920     C                   reset                   dscmz
079100010601     C                   move      invfil        cmzdst
079200990920     C                   eval      cmzfld = vlrfou
079300990920     C                   eval      cmzmbd = vlrhdl
079400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
079500000710     C                   if        prmfir = *blanks
079600150528     C                   eval      cmzfla = 'EDIVAT0F'
079700150528     C                   eval      cmzmba = 'EDIVAT0F'
079800000710     C                   else
079900000710     C                   eval      cmzfla = prmfir
080000000710     C                   eval      cmzmba = prmfir
080100000710     C                   endif
080200990920     C                   eval      cmznrr = *zeros
080300990920     C                   move      §ctrok        cmznrr
080400021018     C                   eval      cmzlba = vlrfl1
080500990920     C                   call(e)   'TIS711C'
080600990920     C                   parm                    dscmz
080700990921     C                   parm      *blanks       esito
080800990920     C                   if        %error
080900990920     C                             or cmzerr = '1'
081000990921     C                             or esito  = '1'
081100000710     C                   eval      wrkesito = '3'
081200990920     C                   endif
081300990920     C*
081400000613     C                   ENDSR
081500990910
081600010601
081700010601
081800010601
081900010601
082000010601      /TITLE Invio dei dati al punto operativo.
082100010601     C     opeini        BEGSR
082200010601     C*
082300010601     C* Inizializzo flag e contatori operativi
082400010601     C                   movel     '0'           flgGiro           1
082500010601     C                   movel     '0'           flgMulti          1
082600010601     C                   movel     '1'           flgStato          1
082700010615     C                   movel     '0'           flgOk             1
082800010601     C                   z-add     *zeros        cntNonEl         10 0
082900010601     C                   z-add     *zeros        depfil            3 0
083000010601     C                   z-add     *zeros        invfil            3 0
083100010601     C*
083200010601     C                   ENDSR
083300010601     C***
083400010601
083500010601
083600050421
083700050421
083800050421     C     *pssr         BEGSR
083900050421     C*
084000050421     C                   eval      wrkesito = '2'
084100050421     C*
084200050421     C                   ENDSR     '*CANCL'
084300050421     C***
084400050421
084500010601
084600010330
084700010330
084800000613     C     *inzsr        BEGSR
084900990910     C*
085000990910     C     *entry        plist
085100990920     C                   parm                    tivlrds
085200990921     C                   parm      wrkesito      esito
085300000724     C                   parm                    prmlit
085400000710     C                   parm                    prmfir
085500010330     C*
085600010330     C* CALCOLA LA DATA CORRENTE
085700010330     C                   time                    wn14             14 0
085800150528     C                   movel     wn14          oracor            6 0          *ORA
085900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
086000010330     C                   z-add     wn8           g08dat
086100010330     C                   z-add     *zeros        g08inv
086200010330     C                   movel     '0'           g08err
086300010330     C                   call      'XSRDA8'
086400010330     C                   parm                    wlbda8
086500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
086600000613     C*
086700000613     C                   ENDSR
086800000613     C***
086900040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
087000180206X;X;X;W_VATNSP;X;X;X;X;W_VATPKB;X;X;X;X;X;X;X;X;X;VATNOT_E;X;X;X;X;X;
087100130528
087200130528
