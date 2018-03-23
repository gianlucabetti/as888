000100150528      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
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
003400180202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
003500180202     d o_Esito         s              1s 0
003600180202
003700030715     D*------------------
003800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003900030715     D*------------------
004000030715     D SkSplitFLD      S             10    DIM(100)
004100030715     D SkSplitCSV      S            256    DIM(100)
004200030715     D CharCSV         S              1
004300030715     D CharTXT         S              1
004400030715     D CharNUM         S              1
004500030715     D posDa           S              3  0 INZ(*zeros)
004600030715     D posA            S              3  0 INZ(*zeros)
004700030715     D i               s              3  0 INZ(1)
004800030715     D wGiro           s              1  0 INZ(*zeros)
004900130625     D w50             s             50
005000130625     D wX              s              3  0
005100130625     D num             c                   const('1234567890 ')
005200130528     D wVATNOT_E       s                   LIKE(VATNOT)
005300180206     D wVATPKB         s              7s 1
005400030822     D*------------------
005500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005600030822     D*------------------
005700030822     D posDaDft        S              3  0 INZ(*zeros)
005800030822     D posADft         S              3  0 INZ(*zeros)
005900030822     D j               s              3  0 INZ(1)
006000030822     D wGiroDft        s              1  0 INZ(*zeros)
006100010330     D*------------------
006200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300010330     D*------------------
006400010330     D WLBDA8          DS                  INZ
006500010330     D  G08DAT                 1      8  0
006600010330     D  G08INV                 9     16  0
006700010330     D  G08ERR                17     17
006800010330     D  G08TGI                18     22  0
006900110222
007000031201     D*------------------
007100031201     D* Costanti
007200031201     D*------------------
007300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500010330
007600010330
007700010330
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100010601     C*
008200010601     C                   exsr      opeini
008300040714     C                   exsr      rwvat
008400010601     C*
008500010601     C                   seton                                        lr
008600010601
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010601     C*--------------------------------------------------------
009300010601     C     PREELA        BEGSR
009400010601     C*
009500010601     C* SE OCCORRE SPEDIRE IN FILIALE
009600010601     C                   if        invfil <> *zeros and
009700010601     C                             flgGiro = '0'
009800010601     C*
009900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010000010601     C                   eval      flgGiro = '1'
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700000616     C*
011800010601     C                   ENDSR
011900010601     C***
012000000613
012100010330
012200010601
012300010601
012400010601
012500010330     C*--------------------------------------------------------
012600150528     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
012700010330     C*--------------------------------------------------------
012800040714     C     RWVAT         BEGSR
012900010330     C*
013000010330     C                   if        not %open(tivin00r)
013100010330     C                   open      tivin00r
013200010330     C                   endif
013300150528     C                   if        not %open(EDIVATwr)
013400150528     C                   open      EDIVATwr
013500010330     C                   endif
013600010330     C*
013700010604     C                   clear                   §CTROK
013800010604     C                   clear                   §CTRMO
013900010604     C                   clear                   §CTRNO
014000040726     C*
014100040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014200040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014300040726     C                   exsr      inzvar
014400040726     C                   exsr      impvat
014500010330     C*
0146001802071    C                   do        *HIVAL
014700010330     C*
014800030822     C                   read      tivin00r                               70
014900010618     C*
015000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015100010618     C                   if        vinflg = '1'
015200010618     C                   eval      flgOk = '1'
015300010618     C                   endif
015400010618     C*
015500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015600040510     C                   z-add     *zeros        wLenVINDTA        4 0
015700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016100040510     C                   z-add     1             wFlgVINDTA
016200040510     C                   leave
016300040510     C                   endif
016400040510     C                   enddo
016500040510     C*
0166001802072    C                   if        vindta > *blanks AND
016700040510     C                             wFlgVINDTA = 1
016800010330     C                   add       1             rrnum
016900010330     C*
0170001802073    C                   if        *in70 = *off and
017100010330     C                             (vinflg = *blanks
017200010330     C                              or vinflg = '0'
017300010330     C                              or vinflg = '2')
017400010330     C*
017500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017600010711     C                   if        vinflg = *blanks or vinflg = '0'
017700010711     C                   clear                   vinmsg
017800010711     C                   endif
017900010601     C*
018000010330     C                   exsr      inzvar
018100040714     C                   exsr      impvat
018200010601     C*
018300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018400040714     C                   eval      depfil = VATLNP
018500010601     C                   exsr      repfil
0186001802074    C                   if        depfil = invfil
018700180207     C                   if        vlrpoi = 999
018800040714     C                   move(P)   invfil        VATFGS
018900021025     C                   else
019000040714     C                   move(P)   vlrpoi        VATFGS
019100021025     C                   endif
019200010601     C*
019300010601     C                   exsr      PREELA
019400010601     C*
019500010604     C* Ebbene...
019600010604     C*
019700030822     C  N31              add       1             §CTROK            7 0
019800030822     C   32              add       1             §CTRMO            7 0
019900030822     C   31              add       1             §CTRNO            7 0
0200001802075    C                   if        wGiro = 2
020100110222     C*
0202001802076    C                   if        not *in30
020300110222     C*
020400130528     C* inserimento tipo record 'E'
0205001802077    C                   if        wVATNOT_E <> *blanks
020600130528     C                   eval      VATNOT = wVATNOT_E
020700130528     C                   eval      VATTRC = 'E'
020800150529     C                   eval      VATCMR = 'GAO'
020900150528     C                   eval      VATCNT = 1
021000180202     C*
0211001802078    C                   if        *in31 = *off
021200180202     C*
021300180202     C* se è la prima volta, imposto i dati di controllo
021400180202     C                   if        old_AAS = 0 and
021500180202     C                             old_LNP = 0 and
021600180202     C                             old_NRS = 0 and
021700180202     C                             old_NSP = 0
021800180202     C                   eval      old_AAS = VATAAS
021900180202     C                   eval      old_LNP = VATLNP
022000180202     C                   eval      old_NRS = VATNRS
022100180202     C                   eval      old_NSP = VATNSP
022200180202     C                   endif
022300180207
022400180205     C* controllo se il rcd che scriverò ha una chiave
022500180202     C* se sì devo aggiornare su VAB il peso della spedizione precedente
0226001802079    C                   if        VATAAS <> old_AAS or
022700180202     C                             VATLNP <> old_LNP or
022800180202     C                             VATNRS <> old_NRS or
022900180202     C                             VATNSP <> old_NSP
023000180202     C                   call      'TITZ90R21'
023100180202     C                   parm                    i_OPZ
023200180202     C                   parm      *blank        i_TLA
023300180202     C                   parm                    old_AAS
023400180202     C                   parm                    old_LNP
023500180202     C                   parm                    old_NRS
023600180202     C                   parm                    old_NSP
023700180202     C                   parm                    old_PKB
023800180202     C                   parm                    o_Esito
023900180206     C*
024000180206     C* adesso è la nuova spedizione da tenere in considerazione
024100180206     C                   eval      old_AAS = VATAAS
024200180206     C                   eval      old_LNP = VATLNP
024300180207
024400180206     C                   eval      old_NSP = VATNSP
024500180206     C                   eval      old_PKB = wVATPKB
024600180202     C*
024700180205     C* se no devo sommare il peso a quello della riga precedente
024800180202     C                   else
024900180206     C                   eval      old_PKB = old_PKB + wVATPKB
0250001802079-   C                   endif
025100180205     C*
025200180202     C                   write     EDIVAT00
025300180202     C**N31              write     EDIVAT00
0254001802078-   C                   endif
025500180202     C*
0256001802077-   C                   endif
0257001802076-   C                   endif
0258001802075-   C                   endif
025900010604     C*
0260001802075    C                   if        *in31 = *off and
026100150528     C                             *in32 = *off
026200010604     C                   eval      vinflg = '1'
026300010604     C                   else
026400010604     C                   eval      vinflg = '2'
0265001802075-   C                   endif
0266001802074-   C                   endif
026700010601     C*
0268001802073-   C                   endif
026900010604     C*
0270001802072e   C                   else
027100010330     C                   eval      vinflg = '1'
0272001802072-   C                   endif
027300010601     C*
027400180207     C* se non ci sono più rcd su TIVIN00R
027500180207     C                   if        *in70
027600180207     C* ma ho una spedizione da aggiornare (ho letto almeno un rcd di TIVIN00R)
027700180207     C                             and old_NSP > 0
027800180207     C* scarico il buffer
027900180207     C                   call      'TITZ90R21'
028000180207     C                   parm                    i_OPZ
028100180207     C                   parm      *blank        i_TLA
028200180207     C                   parm                    old_AAS
028300180207     C                   parm                    old_LNP
028400180207     C                   parm                    old_NRS
028500180207     C                   parm                    old_NSP
028600180207     C                   parm                    old_PKB
028700180207     C                   parm                    o_Esito
028800180207     C                   endif
028900180207     C*
029000010601     C  N70              update    tivin000
029100010330     C*
0292001802071-   C  N70              enddo
029300010601     C
029400010601     C*
029500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600010601     C                   if        cntNonEl = *zeros or
029700010601     C                             flgMulti = '0'
029800010330     C* Se non ci sono record con errori ...
029900010601     C                   if        §ctrno = 0 and
030000010604     C                             §ctrmo = 0 and
030100010601     C                             flgStato <> '2'
030200010330     C* ... restituisco esito OK.
030300010330     C                   eval      wrkesito = '0'
030400010330     C                   else
030500010330     C                   if        §ctrok > 0
030600010330     C                   eval      wrkesito = '1'
030700010330     C                   else
030800010615     C                   if        flgOk = '0'
030900010615     C                   eval      wrkesito = '2'
031000010615     C                   else
031100010615     C                   eval      wrkesito = '6'
031200010615     C                   endif
031300010330     C                   endif
031400010330     C                   endif
031500010601     C                   else
031600010601     C                   eval      wrkesito = '9'
031700010601     C                   endif
031800010330     C*
031900010330     C                   if        %open(tivin00r)
032000010330     C                   close     tivin00r
032100010330     C                   endif
032200150528     C                   if        %open(EDIVATwr)
032300150528     C                   close     EDIVATwr
032400010330     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000010330     C                   if        §ctrok > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100010601
034200010601
034300010601
034400010330     C*----------------------------------------------------*
034500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034600010330     C*----------------------------------------------------*
034700010330     C     INZVAR        BEGSR
034800010330     C*
034900030822     C* Inizializzo variabili di wrk
035000020204     C                   Z-ADD     *zeros        Num5_0            5 0
035100030822     C*
035200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035300150528     C                   CLEAR                   EDIVAT00
035400030822     C                   CLEAR                   SkSplitCSV
035500030822     C                   EVAL      i = 1
035600030822     C                   EVAL      posDa = *zeros
035700030822     C                   EVAL      posA  = *zeros
035800110222     C*
035900130528     C                   MOVEL     *blanks       wVATNOT_E
036000180206     C                   CLEAR                   wVATPKB
036100030822     C*
036200030822     C* Reimposto i valori di default
036300030822     C                   EXSR      DEFCAM
036400030822     C*
036500010330     C                   ENDSR
036600010330     C*----------------------------------------------------*
036700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036800010330     C*----------------------------------------------------*
036900010330     C     DEFCAM        BEGSR
037000030822     C*
037100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037200030715     C* e delimitatore testo.
037300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037600030822     C*
037700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
037800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
037900030715     C                   SETON                                        50
038000030715     C                   ELSE
038100030715     C                   SETOFF                                       50
038200030715     C                   ENDIF
038300030822     C*
038400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038500030822     C                   EVAL      posDaDft = 1
038600030822     C                   EVAL      posADft  = 0
038700030822     C                   EVAL      wGiroDft = 0
038800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
038900030822     C                             posDaDft > 0
039000030822     C*
039100030822     C* Gestisco il 1° giro
039200030822     C                   IF        wGiroDft = 0
039300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039500030822     C* Incremento il contatore dei "giri"
039600030822     C                   EVAL      wGiroDft = 1
039700030822     C                   ELSE
039800030822     C                   EVAL      posDaDft = posADft
039900030822     C                   ENDIF
040000030822     C* Eseguo lo scan x trovare la fine del campo corrente
040100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040200030822     C*
040300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040500030822     C                   IF        posDaDft > 0 AND
040600030822     C                             posADft  > 0
040700030822     C* CCM
040800030822     C                   IF        %subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):1:3)
041100030822     C                             = 'CCM'
041200030822     C                   EVAL      PiStr=%trim(%subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500030822     C                   EXSR      CHKNUM
041600030822     C                   IF        PiInt=*on
041700040714     C                   Z-ADD     PiVal         VATCCM
041800030822     C                   ENDIF
041900030822     C                   ENDIF
042000030822     C* LNP
042100030822     C                   IF        %subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):1:3)
042400030822     C                             = 'LNP'
042500030822     C                   EVAL      PiStr=%trim(%subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800030822     C                   EXSR      CHKNUM
042900030822     C                   IF        PiInt=*on
043000040714     C                   Z-ADD     PiVal         VATLNP
043100030822     C                   ENDIF
043200030822     C                   ENDIF
043300040714     C* TRC
043400030822     C                   IF        %subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):1:3)
043700040714     C                             = 'TRC'
043800040714     C                   EVAL      VATTRC=%trim(%subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044100030822     C                   ENDIF
044200030822     C* ...
044300030822     C                   ENDIF
044400030822     C                   ENDDO
044500020204     C*
044600010330     C                   ENDSR
044700010607     C*----------------------------------------------------*
044800040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
044900010607     C*----------------------------------------------------*
045000040714     C     IMPVAT        BEGSR
045100010607     C*
045200110222     C                   SETOFF                                       313230
045300030515     C*
045400040714     C                   MOVEL     datcor        VATAAS
045500010607     C*
045600030715     C**********
045700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
045800030715     C**********
045900030715     C                   IF        wGiro = *zeros
046000040726     C*
046100040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
046200040726     C                   EVAL      depvinDTA = vinDTA
046300040726     C*
046400040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
046500040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
046600040726     C                                      %trim(intcol(3))
046700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
046800031201     C     minu:maiu     XLATE     vindta        vindta
046900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047000040324     C                   DOW       posDa <= %len(%trimr(vindta))
047100030715     C*
047200030715     C* Gestisco il 1° campo
047300030715     C                   IF        i = 1
047400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
047500030715     C                   EVAL      posDa = 1
047600030715     C* Eseguo lo scan x trovare la fine del primo campo
047700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047800030715     C* A questo punto "estraggo" il campo corrente
047900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048000030715     C                                                    (posA-posDa))
048100030715     C* X i campi successivi al 1°
048200030715     C                   ELSE
048300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
048400030715     C                   EVAL      posDa = posA + 1
048500030715     C* Eseguo lo scan x trovare la fine del campo corrente
048600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048700030715     C* Gestisco l'ultimo campo
048800030715     C                   IF        posA = *zeros
048900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
049000030715     C* A questo punto "estraggo" il campo corrente
049100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049200030715     C                                                    (posA-posDa))
049300030715     C                   LEAVE
049400030715     C                   ELSE
049500030715     C* A questo punto "estraggo" il campo corrente
049600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049700030715     C                                                    (posA-posDa))
049800030715     C                   ENDIF
049900030715     C                   ENDIF
050000030715     C* Incremento il contatore d campo
050100030715     C                   EVAL      i = i +1
050200030715     C                   ENDDO
050300030715     C*
050400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
050500030715     C* delimitazione testo
050600030715     C                   EVAL      i = 1
050700030715     C                   DOW       i < %elem(SkSplitFLD)
050800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
050900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
051000030715     C                   EVAL      i = i + 1
051100030715     C                   ENDDO
051200030715     C                   EVAL      wGiro = 1
051300040726     C* Ripristino la 1° riga originale
051400040726     C                   EVAL      vinDTA = depvinDTA
051500030715     C                   ELSE
051600030715     C                   EVAL      wGiro = 2
051700031204     C**********
051800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
051900031204     C* d separatore campo
052000031204     C**********
052100040726     C                   EVAL      vindta = %trim(vindta)
052200040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
052300040726     C                   EVAL      vindta = %subst(vindta:2)
052400040726     C                   ENDDO
052500031204     C*
052600031204     C                   Z-ADD     *zeros        lunghInput        4 0
052700040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
052800040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
052900040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
053000040726     C                   EVAL      lunghInput = %len(%trim(vindta))
053100040726     C                   ENDDO
053200030715     C**********
053300030715     C* Effettuo lo split del campo dati d input
053400030715     C**********
053500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
053600040324     C                   DOW       posDa <= %len(%trimr(vindta))
053700030715     C*
053800030715     C* Gestisco il 1° campo
053900030715     C                   IF        i = 1
054000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
054100030715     C                   EVAL      posDa = 1
054200030715     C* Eseguo lo scan x trovare la fine del primo campo
054300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
054400030715     C* A questo punto "estraggo" il campo corrente
054500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
054600030715     C                                                    (posA-posDa))
054700030715     C* X i campi successivi al 1°
054800030715     C                   ELSE
054900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
055000030715     C                   EVAL      posDa = posA + 1
055100031204     C* Verifico che nn vi sia il campo nn valorizzato
055200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
055300031204     C* Se campo nn valorizzato skippo al prossimo
055400031204     C                   EVAL      SkSplitCSV(i) = *blanks
055500031204     C                   EVAL      posA  = posA + 1
055600031204     C                   ELSE
055700030715     C* Eseguo lo scan x trovare la fine del campo corrente
055800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055900030715     C* Gestisco l'ultimo campo
056000030715     C                   IF        posA = *zeros
056100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
056200030715     C* A questo punto "estraggo" il campo corrente
056300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
056400030715     C                                                    (posA-posDa))
056500030715     C                   LEAVE
056600030715     C                   ELSE
056700030715     C* A questo punto "estraggo" il campo corrente
056800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
056900030715     C                                                    (posA-posDa))
057000030715     C                   ENDIF
057100031204     C                   ENDIF
057200030715     C                   ENDIF
057300030715     C* Incremento il contatore d campo
057400030715     C                   EVAL      i = i +1
057500030715     C                   ENDDO
057600030715     C*
057700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
057800030715     C* delimitazione testo
057900030715     C                   EVAL      i = 1
058000030715     C                   DOW       i < %elem(SkSplitCSV)
058100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
058200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
058300030715     C                   EVAL      i = i + 1
058400030715     C                   ENDDO
058500030715     C*
058600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
058700030715     C                   EVAL      i = 1
058800030715     C                   DOW       i < %elem(SkSplitFLD)
058900030715     C*
059000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
059200030715     C                             SkSplitCSV(i) <> *blanks
059300030721     C*** ATB
059400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
059500040714     C                   EVAL      VATATB = SkSplitCSV(i)
059600030721     C                   ENDIF
059700040714     C*** TRC
059800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
059900040714     C                   EVAL      VATTRC = SkSplitCSV(i)
060000030715     C                   ENDIF
060100040714     C*** NOT
060200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
060300040714     C                   EVAL      VATNOT = SkSplitCSV(i)
060400030721     C                   ENDIF
060500030721     C*
060600030721     C* Reperisco quindi i campi numerici...
060700030721     C*** NSP
060800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
060900030721     C                   EVAL      PiStr=SkSplitCSV(i)
061000030721     C                   EXSR      CHKNUM
061100040726     C                   IF        PiInt=*on
061200040726     C                   Z-ADD     PiVal         VATNSP
061300040802     C*                  IF        PiVal>9999999
061400040802     C*                  SETON                                        32
061500040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
061600040802     C*                            + ' ' + 'VATNSP'
061700040802     C*                  ENDIF
061800030721     C                   ELSE
061900071002     C                   SETON                                        32
062000040714     C                   EVAL      VATNSP = *zeros
062100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
062200040714     C                             + ' ' + 'VATNSP'
062300030721     C                   ENDIF
062400030721     C                   ENDIF
062500030721     C*** CCM
062600040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
062700030721     C                   EVAL      PiStr=SkSplitCSV(i)
062800030721     C                   EXSR      CHKNUM
062900030721     C                   IF        PiInt=*on      AND
063000030721     C                             PiVal<=9999999 AND
063100030721     C                             PiVal>*zeros
063200040714     C                   Z-ADD     PiVal         VATCCM
063300030721     C                   ELSE
063400030721     C                   SETON                                        32
063500040714     C                   EVAL      VATCCM = *zeros
063600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
063700040714     C                             + ' ' + 'VATCCM'
063800030721     C                   ENDIF
063900030721     C                   ENDIF
064000030721     C*** LNP
064100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
064200030721     C                   EVAL      PiStr=SkSplitCSV(i)
064300030721     C                   EXSR      CHKNUM
064400030721     C                   IF        PiInt=*on    AND
064500030721     C                             PiVal<=999   AND
064600030721     C                             PiVal>*zeros
064700040714     C                   Z-ADD     PiVal         VATLNP
064800030721     C                   ELSE
064900030721     C                   SETON                                        32
065000040714     C                   EVAL      VATLNP = *zeros
065100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
065200040714     C                             + ' ' + 'VATLNP'
065300030721     C                   ENDIF
065400030721     C                   ENDIF
065500030721     C*** NRS
065600040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
065700030721     C                   EVAL      PiStr=SkSplitCSV(i)
065800030721     C                   EXSR      CHKNUM
065900030721     C                   IF        PiInt=*on AND
066000030721     C                             PiVal<=99
066100040714     C                   Z-ADD     PiVal         VATNRS
066200030721     C                   ELSE
066300030721     C                   SETON                                        32
066400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
066500040714     C                             + ' ' + 'VATNRS'
066600030721     C                   ENDIF
066700030721     C                   ENDIF
066800130528     C*** VATNOT_E
066900130528     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
067000130528     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
067100130528     C                   ENDIF
067200110222     C*
067300110222     C***  ===>  Gestione campi "particolari" dentro ciclo
067400130625      *** W_VATNSP: è sia VABRMA che VABNSP ma solo gli ultimi char a dx numerici
067500130625     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNSP'
067600130625     C                   EVAL      w50 = %trim(SkSplitCSV(i))
067700130625     C* trovo l'ultimo carattere non numerico e non blank
067800130625     C                   EVAL      wX = %checkR(num : W50) + 1
067900130625     C                   EVAL      VATNSP = %dec(
068000130625     C                                   %subst(w50:wX:
068100130625     C                                    %scan(' ':w50)-wX)
068200130625     C                                                 :7:0)
068300130528     C                   ENDIF
068400180206     C*
068500180206     C*** PKB
068600180206     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATPKB'
068700180206     C                   EVAL      PiStr=SkSplitCSV(i)
068800180206     C                   EXSR      CHKNUM
068900180206     C                   IF        PiNum=*on
069000180206     C                   EVAL(H)   wVATPKB = PiVal
069100180206     C                   ELSE
069200180206     C                   EVAL      wVATPKB = *zeros
069300180206     C                   ENDIF
069400180206     C                   ENDIF
069500110222     C*
069600110222     C***  <===  -----------------------------------------
069700030715     C*
069800030715     C                   ENDIF
069900030715     C                   EVAL      i = i + 1
070000030715     C                   ENDDO
070100180206     C*
070200180206     C***  ===>  Gestione campi "particolari" fuori ciclo
070300040726     C*
070400180206     C*
070500180206     C***  <===  -----------------------------------------
070600180206     C*
070700040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
070800040726     C                   IF        VATNSP = *zeros
070900040726     C                   SETON                                        31
071000040726     C                   ENDIF
071100040726     C*
071200040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
071300040726     C                   IF        VATLNP = *zeros
071400040726     C                   SETON                                        31
071500040726     C                   ENDIF
071600020204     C*
071700030715     C                   ENDIF
071800020204     C*
071900010607     C                   ENDSR
072000010607     C*----------------------------------------------------*
072100040714
072200010330
072300010330
072400010330     C*----------------------------------------------------*
072500010330     C*  CONTROLLO NUMERICITA' CAMPI
072600010330     C*----------------------------------------------------*
072700010330     C     CHKNUM        BEGSR
072800010330     C*
072900010606     C                   IF        PiDecChr = *blanks
073000030715     C                   EVAL      PiDecChr = CharNUM
073100010606     C                   ENDIF
073200010606     C*
073300010606     C                   CALL(e)   'ISNUMERIC'
073400010330     C                   PARM                    PiStr            30
073500010606     C                   PARM                    PiDecChr          1
073600010330     C                   PARM      *ZEROS        PiVal            30 9
073700010330     C                   PARM      '0'           PiInt             1
073800010330     C                   PARM      '0'           PiNum             1
073900010330     C                   IF        %error
074000010606     C                   EVAL      PiNum=*off
074100010330     C                   ENDIF
074200010330     C*
074300010330     C                   ENDSR
074400010330     C***
074500010330
074600010601
074700010601
074800010601
074900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
075000010601     C     repfil        BEGSR
075100010601     C*
075200010601     C                   if        invfil = *zeros and
075300010601     C                             depfil > *zeros and
075400010629     C                             (vinflg = *blanks or
075500010629     C                              vinflg = *zeros)
075600010601     C
075700010601     C                   eval      invfil = depfil
075800010601     C                   endif
075900010601     C*
076000010601     C                   if        depfil <> invfil and
076100010601     C                             invfil > *zeros
076200010601     C                   eval      flgMulti = '1'
076300010601     C                   if        vinflg = *blanks
076400010601     C                   add       1             cntNonEl
076500010601     C                   endif
076600010601     C                   endif
076700010601     C*
076800010601     C                   if        vinflg = '2'
076900010601     C                   eval      flgStato = '2'
077000010601     C                   endif
077100010601     C*
077200010601     C                   ENDSR
077300010601     C***
077400010601
077500010601
077600010601
077700010330
077800010330
077900010330
078000990920      /TITLE Invio dei dati al punto operativo.
078100000613     C     invio         BEGSR
078200990920     C*
078300990920     C                   reset                   dscmz
078400010601     C                   move      invfil        cmzdst
078500990920     C                   eval      cmzfld = vlrfou
078600990920     C                   eval      cmzmbd = vlrhdl
078700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
078800000710     C                   if        prmfir = *blanks
078900150528     C                   eval      cmzfla = 'EDIVAT0F'
079000150528     C                   eval      cmzmba = 'EDIVAT0F'
079100000710     C                   else
079200000710     C                   eval      cmzfla = prmfir
079300000710     C                   eval      cmzmba = prmfir
079400000710     C                   endif
079500990920     C                   eval      cmznrr = *zeros
079600990920     C                   move      §ctrok        cmznrr
079700021018     C                   eval      cmzlba = vlrfl1
079800990920     C                   call(e)   'TIS711C'
079900990920     C                   parm                    dscmz
080000990921     C                   parm      *blanks       esito
080100990920     C                   if        %error
080200990920     C                             or cmzerr = '1'
080300990921     C                             or esito  = '1'
080400000710     C                   eval      wrkesito = '3'
080500990920     C                   endif
080600990920     C*
080700000613     C                   ENDSR
080800990910
080900010601
081000010601
081100010601
081200010601
081300010601      /TITLE Invio dei dati al punto operativo.
081400010601     C     opeini        BEGSR
081500010601     C*
081600010601     C* Inizializzo flag e contatori operativi
081700010601     C                   movel     '0'           flgGiro           1
081800010601     C                   movel     '0'           flgMulti          1
081900010601     C                   movel     '1'           flgStato          1
082000010615     C                   movel     '0'           flgOk             1
082100010601     C                   z-add     *zeros        cntNonEl         10 0
082200010601     C                   z-add     *zeros        depfil            3 0
082300010601     C                   z-add     *zeros        invfil            3 0
082400010601     C*
082500010601     C                   ENDSR
082600010601     C***
082700010601
082800010601
082900050421
083000050421
083100050421     C     *pssr         BEGSR
083200050421     C*
083300050421     C                   eval      wrkesito = '2'
083400050421     C*
083500050421     C                   ENDSR     '*CANCL'
083600050421     C***
083700050421
083800010601
083900010330
084000010330
084100000613     C     *inzsr        BEGSR
084200990910     C*
084300990910     C     *entry        plist
084400990920     C                   parm                    tivlrds
084500990921     C                   parm      wrkesito      esito
084600000724     C                   parm                    prmlit
084700000710     C                   parm                    prmfir
084800010330     C*
084900010330     C* CALCOLA LA DATA CORRENTE
085000010330     C                   time                    wn14             14 0
085100150528     C                   movel     wn14          oracor            6 0          *ORA
085200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
085300010330     C                   z-add     wn8           g08dat
085400010330     C                   z-add     *zeros        g08inv
085500010330     C                   movel     '0'           g08err
085600010330     C                   call      'XSRDA8'
085700010330     C                   parm                    wlbda8
085800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
085900000613     C*
086000000613     C                   ENDSR
086100000613     C***
086200040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
086300180206X;X;X;W_VATNSP;X;X;X;X;W_VATPKB;X;X;X;X;X;X;X;X;X;VATNOT_E;X;X;X;X;X;
086400130528
086500130528
