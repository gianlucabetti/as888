000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400161018     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140117     FEDIVABwr  O    E             DISK    usropn
000700140117     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070719     D tisi95ds      e ds
001700161018     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700141028     D wTag            s            128    VARYING INZ
002800141028     D wRecErr         s              1  0 INZ(*zeros)
002900141028     D wFlgCAS         s              1    INZ(*blanks)
003000141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003100161013     D wEMAIL          s             70    INZ(*blanks)
003200141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003900161018
004000161018     D*------------
004100161018     D jNAZ            s              5  0 INZ(*zeros)
004200161018     D skNAZISO        S              3    DIM(1000)
004300161018     D skNAZBAR        S              3    DIM(1000)
004400141028
004500141028     D*------------------
004600141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700141028     D*------------------
004800141028     D CharNUM         S              1
004900141028     D CharSOS         S              1
005000141028     D posDaDft        S              3  0 INZ(*zeros)
005100141028     D posADft         S              3  0 INZ(*zeros)
005200141028     D wGiroDft        s              1  0 INZ(*zeros)
005300141028
005400141028
005500141028     D*------------------
005600141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005700141028     D*------------------
005800141028     D InStringa       S          65535A   VARYING                              (stringa input)
005900141028     D InSepara        S             10A                                        (separatore)
006000141028     D InTypeOut       S              1A                                        (tipo output)
006100141028     D wSkParam        S          65535A                                        (output)
006200141028     D OutErrore       S              1A                                        (flag errore)
006300141028     D OutMsg          S             80A                                        (messaggio errore)
006400171011     D SkSplitCSV_4    S            128    DIM(512)
006500141028     D SkSplitCSV_5    S            256    DIM(256)
006600141028
006700000830
006800041025     D*------------------
006900041025     D* DS REPERIMENTO NUMERATORE
007000041025     D*------------------
007100041025     D trul33ds      e ds                  inz
007200041025     D*------------------
007300041025     D* DS ARCHITETTURA
007400041025     D*------------------
007500041025     D kpjba         e ds                  inz
007600041025     D*------------------
007700990908
007800141028
007900141028     D*------------------
008000141028     D* LINKING A DEFINIZIONI ESTERNE
008100141028     D*------------------
008200141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008300141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400150609
008500141028
008600150609     D*-------------------
008700150609     D* COSTANTI
008800150609     D*-------------------
008900150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009000150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009100141028
009200010201
009300010201
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600990921     C                   reset                   wrkesito
009700000613     C*
009800161018     C                   EXSR      CARTAB                                       CARICA TABELLE
009900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010000070719     C*
010100070719     C* Esegue lancio TISI95R solo x chiusura
010200070719     C                   CLEAR                   TISI95DS
010300070719     C                   EVAL      I95TLA = 'C'
010400070719     C                   CALL      'TISI95R'
010500070719     C                   PARM                    TISI95DS
010600000613     C*
010700010202     C* Effettuo la chiamata al CLLE preposto
010800140922     C                   call(e)   'TITVEVTC'
010900140922     C                   parm                    parccm
011000140922     C                   parm                    parmbr
011100140922     C                   parm      '2'           paropz
011200000616     C*
011300010201     C                   seton                                        LR
011400990908
011500000801
011600910830     C*--------------------------------------------------------
011700140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011800910830     C*--------------------------------------------------------
011900040526     C     RWFILE        BEGSR
012000990910     C*
012100990914     C                   if        not %open(tivin00r)
012200990908     C                   open      tivin00r
012300990914     C                   endif
012400140117     C                   if        not %open(edivabwr)
012500140117     C                   open      edivabwr
012600990914     C                   endif
012700141028     C*
012800140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012900020305     C                   exsr      prevat
013000010201     C*
013100010202     C                   if        chkcall = '0'
013200010202     C*
013300140117     C                   if        not %open(edivatwr)
013400140117     C                   open      edivatwr
013500010201     C                   endif
013600990910     C*
013700010201     C                   clear                   §CTROKVB          5 0
013800020305     C                   clear                   §CTROKVT          5 0
013900000801     C                   clear                   §CTRMO            5 0
014000000801     C                   clear                   §CTRNO            5 0
014100171011     C*
014200171011     C* Inizializzazioni fuori ciclo
014300171011     C                   SETOFF                                       40
014400171011     C                   EXSR      INZVAR
014500130708     C*
014600921023     C                   DO        *HIVAL
014700990913     C*
014800990915     C                   READ      tivin00r                               70
014900040910     C                   if        vindta > *blanks
015000000613     C                   add       1             rrnum
015100000801     C*
015200000801     C                   if        *in70 = *off
015300000801     C                             and
015400000801     C                             (vinflg = *blanks
015500000801     C                              or vinflg = '0'
015600000801     C                              or vinflg = '2')
015700000801     C*
015800000801     C                   clear                   vinmsg
015900141028     C*
016000141028     C* "normalizzo" il dato di input
016100141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
016200141028     C                                             %len(%trim(vindta))-1))
016300160209     C*
016400160209     C* Elimino le "duple di controllo EDIFACT"
016500160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
016600160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
016700160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
016800160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
016900160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
017000040910     C*
017100040910     C* Eseguo routine d traduzione
017200040910     C                   exsr      impvabvat
017300040802     C*
017400010305     C                   endif
017500000905     C*
017600000905     C                   else
017700000905     C                   eval      vinflg = '1'
017800000905     C                   endif
017900000905     C*
018000000905     C  N70              update    tivin000
018100000905     C*
018200991022     C  N70              ENDdo
018300010202     C*
018400010202     C                   endif
018500171011     C*
018600990910     C* Se non ci sono record con errori ...
018700000710     C                   if        §ctrno = 0
018800990910     C* ... restituisco esito OK.
018900990921     C                   eval      wrkesito = '0'
019000990910     C                   else
019100010201     C                   if        §ctrokvb > 0
019200990921     C                   eval      wrkesito = '1'
019300000710     C                   else
019400000710     C                   eval      wrkesito = '2'
019500990910     C                   endif
019600000710     C                   endif
019700990910     C*
019800990914     C                   if        %open(tivin00r)
019900990908     C                   close     tivin00r
020000990914     C                   endif
020100140117     C                   if        %open(edivabwr)
020200140117     C                   close     edivabwr
020300990914     C                   endif
020400140117     C                   if        %open(edivatwr)
020500140117     C                   close     edivatwr
020600010201     C                   endif
020700990910     C*
020800010201     C                   if        §ctrokvb > 0
020900000724     C                             and vlrpoi <> *zeros
021000010202     C                   exsr      invio
021100990920     C                   endif
021200990920     C*
021300910830     C                   ENDSR
021400000613     C***
021500140922
021600140922     C*----------------------------------------------------*
021700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
021800140922     C*----------------------------------------------------*
021900140922     C     CHKWRI        BEGSR
022000140922     C*
022100141028     C                   if        VABRMN = *zeros
022200141028     C                   eval      VABRMN = VABNSP
022300141028     C                   endif
022400170214     C*
022500170214     C* Se peso non indicato nei dati cleinte e richiesto un peso forzato per collo
022600170214     C                   IF        VABPKB = *zeros
022700170214     C                   EVAL      VABPKB = wVABPKN * VABNCL
022800170214     C                   ENDIF
022900141028     C*
023000141028     C                   if        VABPRD = *blanks
023100141028     C* ......VABPRD
023200141028     C* Reperisco la provincia dal CAP e dalla località
023300141028     C                   IF        VABLOD <> *blanks AND
023400141028     C                             VABCAD <> *blanks AND
023500141028     C                             VABNZD  = *blanks
023600141028     C                   CLEAR                   TISI95DS
023700141028     C                   EVAL      I95TCN = '3'
023800141028     C                   Z-ADD     datcor        I95DAT
023900141028     C                   EVAL      I95CAP = VABCAD
024000141028     C                   EVAL      I95LOC = VABLOD
024100141028     C                   CALL      'TISI95R'
024200141028     C                   PARM                    TISI95DS
024300141028     C                   EVAL      VABPRD = O95PRV
024400141028     C                   ENDIF
024500141028     C                   endif
024600140922     C*
024700140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
024800141028     C                   IF        wFlgCAS <> '0'
024900140922     C                   IF        VABCBO = '1'
025000140922     C                   EVAL      VABCBO = '4'
025100140922     C                   ELSE
025200140922     C                   EVAL      VABCBO = '6'
025300140922     C                   ENDIF
025400140922     C                   ENDIF
025500140922     C*
025600140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
025700141028     C                   EXSR      CHKIMPDIV
025800140922     C*
025900140922     C                   ENDSR
026000141028     C*----------------------------------------------------*
026100141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026200141028     C*----------------------------------------------------*
026300141028     C     INZVAR        BEGSR
026400141028     C*
026500141028     C* Inizializzo variabili di wrk
026600141028     C                   Z-ADD     *zeros        Num5_0            5 0
026700141028     C                   MOVEL     '0'           wFlgCAS
026800141028     C                   MOVEL     *blanks       wVATNOT_A
026900141028     C                   MOVEL     *blanks       wVATNOT_B
027000141028     C                   MOVEL     *blanks       wVATNOT_E
027100141028     C                   MOVEL     *blanks       wVATNOT_I
027200141028     C                   MOVEL     *blanks       wVATNOT_J
027300141028     C                   MOVEL     *blanks       wVATNOT_S
027400141028     C                   MOVEL     *blanks       wVATNOT_P
027500171011     C                   CLEAR                   SkSplitCSV_4
027600141028     C                   CLEAR                   SkSplitCSV_5
027700141028     C*
027800141028     C* Reimposto i valori di default
027900141028     C                   EXSR      DEFCAM
028000141028     C*
028100141028     C                   ENDSR
028200141028     C*----------------------------------------------------*
028300141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028400141028     C*----------------------------------------------------*
028500141028     C     DEFCAM        BEGSR
028600141028     C*
028700141028     C                   CLEAR                   EDIVAB00
028800141028     C                   CLEAR                   EDIVAT00
028900141028     C*
029000141028     C* Imposto i valori bolla di default
029100141028     C                   EVAL      VABCTR = 000
029200141028     C                   EVAL      VABCBO = '1'
029300141104     C                   EVAL      VABTSP = 'C'
029400141028     C*
029500141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029600141028     C* e delimitatore testo.
029700141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029800141028     C*
029900141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030000141028     C                   EVAL      CharSOS = CharNUM
030100141028     C*
030200141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030300141028     C                   EVAL      posDaDft = 1
030400141028     C                   EVAL      posADft  = 0
030500141028     C                   EVAL      wGiroDft = 0
030600141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
030700141028     C                             posDaDft > 0
030800141028     C*
030900141028     C* Gestisco il 1° giro
031000141028     C                   IF        wGiroDft = 0
031100141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
031200141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031300141028     C* Incremento il contatore dei "giri"
031400141028     C                   EVAL      wGiroDft = 1
031500141028     C                   ELSE
031600141028     C                   EVAL      posDaDft = posADft
031700141028     C                   ENDIF
031800141028     C* Eseguo lo scan x trovare la fine del campo corrente
031900141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032000141028     C*
032100141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
032200141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
032300141028     C                   IF        posDaDft > 0 AND
032400141028     C                             posADft  > 0
032500141028     C* NCL
032600141028     C                   IF        %subst(
032700141028     C                             %subst(vlrppt:posDaDft+1:
032800141028     C                             posADft-posDaDft-1):1:3)
032900141028     C                             = 'NCL'
033000141028     C                   EVAL      PiStr=%trim(%subst(
033100141028     C                             %subst(vlrppt:posDaDft+1:
033200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033300141028     C                   EXSR      CHKNUM
033400141028     C                   IF        PiInt=*on
033500141028     C                   Z-ADD     PiVal         VABNCL
033600141028     C                   ENDIF
033700141028     C                   ENDIF
033800141028     C* CCM
033900141028     C                   IF        %subst(
034000141028     C                             %subst(vlrppt:posDaDft+1:
034100141028     C                             posADft-posDaDft-1):1:3)
034200141028     C                             = 'CCM'
034300141028     C                   EVAL      PiStr=%trim(%subst(
034400141028     C                             %subst(vlrppt:posDaDft+1:
034500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034600141028     C                   EXSR      CHKNUM
034700141028     C                   IF        PiInt=*on
034800141028     C                   Z-ADD     PiVal         VABCCM
034900141028     C                   ENDIF
035000141028     C                   ENDIF
035100141028     C* LNP
035200141028     C                   IF        %subst(
035300141028     C                             %subst(vlrppt:posDaDft+1:
035400141028     C                             posADft-posDaDft-1):1:3)
035500141028     C                             = 'LNP'
035600141028     C                   EVAL      PiStr=%trim(%subst(
035700141028     C                             %subst(vlrppt:posDaDft+1:
035800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035900141028     C                   EXSR      CHKNUM
036000141028     C                   IF        PiInt=*on
036100141028     C                   Z-ADD     PiVal         VABLNP
036200141028     C                   ENDIF
036300141028     C                   ENDIF
036400141028     C* NRS
036500141028     C                   IF        %subst(
036600141028     C                             %subst(vlrppt:posDaDft+1:
036700141028     C                             posADft-posDaDft-1):1:3)
036800141028     C                             = 'NRS'
036900141028     C                   EVAL      PiStr=%trim(%subst(
037000141028     C                             %subst(vlrppt:posDaDft+1:
037100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037200141028     C                   EXSR      CHKNUM
037300141028     C                   IF        PiInt=*on
037400141028     C                   Z-ADD     PiVal         VABNRS
037500141028     C                   ENDIF
037600141028     C                   ENDIF
037700141028     C* CTR
037800141028     C                   IF        %subst(
037900141028     C                             %subst(vlrppt:posDaDft+1:
038000141028     C                             posADft-posDaDft-1):1:3)
038100141028     C                             = 'CTR'
038200141028     C                   EVAL      PiStr=%trim(%subst(
038300141028     C                             %subst(vlrppt:posDaDft+1:
038400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038500141028     C                   EXSR      CHKNUM
038600141028     C                   IF        PiInt=*on
038700141028     C                   Z-ADD     PiVal         VABCTR
038800141028     C                   ENDIF
038900141028     C                   ENDIF
039000141028     C* PKB
039100141028     C                   IF        %subst(
039200141028     C                             %subst(vlrppt:posDaDft+1:
039300141028     C                             posADft-posDaDft-1):1:3)
039400141028     C                             = 'PKB'
039500141028     C                   EVAL      PiStr=%trim(%subst(
039600141028     C                             %subst(vlrppt:posDaDft+1:
039700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039800141028     C                   EXSR      CHKNUM
039900141028     C                   IF        PiNum=*on
040000141028     C                   Z-ADD     PiVal         VABPKB
040100141028     C                   ENDIF
040200141028     C                   ENDIF
040300141028     C* VLB
040400141028     C                   IF        %subst(
040500141028     C                             %subst(vlrppt:posDaDft+1:
040600141028     C                             posADft-posDaDft-1):1:3)
040700141028     C                             = 'VLB'
040800141028     C                   EVAL      PiStr=%trim(%subst(
040900141028     C                             %subst(vlrppt:posDaDft+1:
041000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041100141028     C                   EXSR      CHKNUM
041200141028     C                   IF        PiNum=*on
041300141028     C                   Z-ADD     PiVal         VABVLB
041400141028     C                   ENDIF
041500141028     C                   ENDIF
041600141028     C* QFT
041700141028     C                   IF        %subst(
041800141028     C                             %subst(vlrppt:posDaDft+1:
041900141028     C                             posADft-posDaDft-1):1:3)
042000141028     C                             = 'QFT'
042100141028     C                   EVAL      PiStr=%trim(%subst(
042200141028     C                             %subst(vlrppt:posDaDft+1:
042300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400141028     C                   EXSR      CHKNUM
042500141028     C                   IF        PiNum=*on
042600141028     C                   Z-ADD     PiVal         VABQFT
042700141028     C                   ENDIF
042800141028     C                   ENDIF
042900141028     C* CBO
043000141028     C                   IF        %subst(
043100141028     C                             %subst(vlrppt:posDaDft+1:
043200141028     C                             posADft-posDaDft-1):1:3)
043300141028     C                             = 'CBO'
043400141028     C                   EVAL      VABCBO=%trim(%subst(
043500141028     C                             %subst(vlrppt:posDaDft+1:
043600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700141028     C                   ENDIF
043800141028     C* TSP
043900141028     C                   IF        %subst(
044000141028     C                             %subst(vlrppt:posDaDft+1:
044100141028     C                             posADft-posDaDft-1):1:3)
044200141028     C                             = 'TSP'
044300141028     C                   EVAL      VABTSP=%trim(%subst(
044400141028     C                             %subst(vlrppt:posDaDft+1:
044500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044600141028     C                   ENDIF
044700141028     C* VAS
044800141028     C                   IF        %subst(
044900141028     C                             %subst(vlrppt:posDaDft+1:
045000141028     C                             posADft-posDaDft-1):1:3)
045100141028     C                             = 'VAS'
045200141028     C                   EVAL      VABVAS=%trim(%subst(
045300141028     C                             %subst(vlrppt:posDaDft+1:
045400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045500141028     C                   ENDIF
045600141028     C* VCA
045700141028     C                   IF        %subst(
045800141028     C                             %subst(vlrppt:posDaDft+1:
045900141028     C                             posADft-posDaDft-1):1:3)
046000141028     C                             = 'VCA'
046100141028     C                   EVAL      VABVCA=%trim(%subst(
046200141028     C                             %subst(vlrppt:posDaDft+1:
046300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400141028     C                   ENDIF
046500141028     C* TIC
046600141028     C                   IF        %subst(
046700141028     C                             %subst(vlrppt:posDaDft+1:
046800141028     C                             posADft-posDaDft-1):1:3)
046900141028     C                             = 'TIC'
047000141028     C                   EVAL      VABTIC=%trim(%subst(
047100141028     C                             %subst(vlrppt:posDaDft+1:
047200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300141028     C                   ENDIF
047400141028     C* GCA
047500141028     C                   IF        %subst(
047600141028     C                             %subst(vlrppt:posDaDft+1:
047700141028     C                             posADft-posDaDft-1):1:3)
047800141028     C                             = 'GCA'
047900141028     C                   EVAL      VABGCA=%trim(%subst(
048000141028     C                             %subst(vlrppt:posDaDft+1:
048100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048200141028     C                   ENDIF
048300141028     C* CTM
048400141028     C                   IF        %subst(
048500141028     C                             %subst(vlrppt:posDaDft+1:
048600141028     C                             posADft-posDaDft-1):1:3)
048700141028     C                             = 'CTM'
048800141028     C                   EVAL      VABCTM=%trim(%subst(
048900141028     C                             %subst(vlrppt:posDaDft+1:
049000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049100141028     C                   ENDIF
049200141028     C* FFD
049300141028     C                   IF        %subst(
049400141028     C                             %subst(vlrppt:posDaDft+1:
049500141028     C                             posADft-posDaDft-1):1:3)
049600141028     C                             = 'FFD'
049700141028     C                   EVAL      VABFFD=%trim(%subst(
049800141028     C                             %subst(vlrppt:posDaDft+1:
049900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000141028     C                   ENDIF
050100141028     C* VAD
050200141028     C                   IF        %subst(
050300141028     C                             %subst(vlrppt:posDaDft+1:
050400141028     C                             posADft-posDaDft-1):1:3)
050500141028     C                             = 'VAD'
050600141028     C                   EVAL      VABVAD=%trim(%subst(
050700141028     C                             %subst(vlrppt:posDaDft+1:
050800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050900141028     C                   ENDIF
051000141028     C* GMA
051100141028     C                   IF        %subst(
051200141028     C                             %subst(vlrppt:posDaDft+1:
051300141028     C                             posADft-posDaDft-1):1:3)
051400141028     C                             = 'GMA'
051500141028     C                   EVAL      VABGMA=%trim(%subst(
051600141028     C                             %subst(vlrppt:posDaDft+1:
051700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800141028     C                   ENDIF
051900141028     C* GGA
052000141028     C                   IF        %subst(
052100141028     C                             %subst(vlrppt:posDaDft+1:
052200141028     C                             posADft-posDaDft-1):1:3)
052300141028     C                             = 'GGA'
052400141028     C                   EVAL      VABGGA=%trim(%subst(
052500141028     C                             %subst(vlrppt:posDaDft+1:
052600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700141028     C                   ENDIF
052800141028     C* GVA
052900141028     C                   IF        %subst(
053000141028     C                             %subst(vlrppt:posDaDft+1:
053100141028     C                             posADft-posDaDft-1):1:3)
053200141028     C                             = 'GVA'
053300141028     C                   EVAL      VABGVA=%trim(%subst(
053400141028     C                             %subst(vlrppt:posDaDft+1:
053500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600141028     C                   ENDIF
053700141028     C* TC1
053800141028     C                   IF        %subst(
053900141028     C                             %subst(vlrppt:posDaDft+1:
054000141028     C                             posADft-posDaDft-1):1:3)
054100141028     C                             = 'TC1'
054200141028     C                   EVAL      VABTC1=%trim(%subst(
054300141028     C                             %subst(vlrppt:posDaDft+1:
054400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500141028     C                   ENDIF
054600141028     C* TC2
054700141028     C                   IF        %subst(
054800141028     C                             %subst(vlrppt:posDaDft+1:
054900141028     C                             posADft-posDaDft-1):1:3)
055000141028     C                             = 'TC2'
055100141028     C                   EVAL      VABTC2=%trim(%subst(
055200141028     C                             %subst(vlrppt:posDaDft+1:
055300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400141028     C                   ENDIF
055500141028     C* VATTRC
055600141028     C                   IF        %subst(
055700141028     C                             %subst(vlrppt:posDaDft+1:
055800141028     C                             posADft-posDaDft-1):1:3)
055900141028     C                             = 'TRC'
056000141028     C                   EVAL      VATTRC=%trim(%subst(
056100141028     C                             %subst(vlrppt:posDaDft+1:
056200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300141028     C                   ENDIF
056400170214     C*
056500170214     C***
056600170214     C* Campi "personalizzati"
056700170214     C***
056800170214     C* PKN
056900170214     C                   IF        %subst(
057000170214     C                             %subst(vlrppt:posDaDft+1:
057100170214     C                             posADft-posDaDft-1):1:3)
057200170214     C                             = 'PKN'
057300170214     C                   EVAL      PiStr=%trim(%subst(
057400170214     C                             %subst(vlrppt:posDaDft+1:
057500170214     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600170214     C                   EXSR      CHKNUM
057700170214     C                   IF        PiNum=*on
057800170214     C                   Z-ADD     PiVal         wVABPKN           7 1
057900170214     C                   ENDIF
058000170214     C                   ENDIF
058100170214     C***
058200141028     C* ...
058300141028     C                   ENDIF
058400141028     C                   ENDDO
058500141028     C*
058600141028     C                   ENDSR
058700000801     C*----------------------------------------------------*
058800140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
058900000801     C*----------------------------------------------------*
059000040910     C     IMPVABVAT     BEGSR
059100040910     C*
059200040910     C* Traduzione relativa ai tipi record del file del cliente
059300040910     C*
059400071210     C*
059500071210     C***
059600141028     C* ...tipo record 'UNB' (info CMR)
059700141028     C                   EVAL      wTag = 'UNB'
059800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
059900141028     C                   MOVEL     *blanks       savCMR           35
060000141028     C                   EVAL      InStringa =
060100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
060200141028     C                   EVAL      InSepara = '+'
060300171011     C                   EXSR      SR_SPLIT5
060400141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
060500140922     C                                      %char(datcor) + ' ' + %char(oracor)
060600130620     C                   ENDIF
060700141028     C*
060800141028     C***
060900171011     C* ...tipo record 'GID' (nuova spedizione)
061000171011     C                   EVAL      wTag = 'GID'
061100171011     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061200171011     C*
061300171011     C* Scarico testata in sospeso
061400171011     C                   IF        *in40
061500171011     C  N31              ADD       1             §CTROKVB
061600171011     C  N31              exsr      CHKWRI
061700171011     C  N31              WRITE     EDIVAB00
061800171011     C                   ELSE
061900171011     C                   SETON                                        40
062000171011     C                   ENDIF
062100171011     C*
062200171011     C* Inizio nuova bolla
062300171011     C                   SETOFF                                       31
062400141028     C                   eval      vinflg = '1'
062500141028     C* ......inizializzazioni iniziali e formati record file Bartolini
062600141028     C                   EXSR      INZVAR
062700141028     C* ......valorizzazione campi fissi
062800141028     C                   MOVEL     datcor        VABAAS
062900141028     C                   MOVE      datcor        VABMGS
063000141028     C                   MOVE(P)   vlrpoi        VABFGS
063100141028     C                   EVAL      VABCMR = savCMR
063200141028     C                   EVAL      VABDCM = datcor
063300141028     C                   EVAL      VABDTS = datcor
063400141028     C                   EVAL      VABCNT = 1
063500141028     C* ......NSP => Stacco un numeratore da AZNUM
063600141028     C                   clear                   TRUL33DS
063700141028     C                   eval      I33OPE = *zeros
063800141028     C                   eval      I33CNU = 302
063900141028     C                   eval      I33NUM = 1
064000141028     C                   movel     TRUL33DS      KPJBU
064100141028     C                   call      'TRUL33R'
064200141028     C                   parm                    KPJBA
064300141028     C                   movel     KPJBU         TRUL33DS
064400141028     C                   if        O33ERR = *zeros
064500141028     C                   z-add     O33NRF        VABNSP
064600141028     C                   else
064700141028     C                   Z-ADD     1             wRecErr
064800141028     C                   SETON                                        31
064900141028     C                   EVAL      vinmsg = %trimr(vinmsg)
065000141028     C                             + ' ' + 'VABNSP VATNSP'
065100141028     C                   endif
065200141028     C                   ENDIF
065300161025     C*
065400161025     C***
065500171011     C* ...tipo record 'MEA+AAE+AAB+' (peso spedizione)
065600171011     C                   EVAL      wTag = 'MEA+AAE+AAB'
065700171011     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
065800171011     C                   EVAL      InStringa =
065900171011     C                                %subst(wvindta:%len(%trim(wTag))+2)
066000171011     C                   EVAL      InSepara = ':'
066100171011     C                   EXSR      SR_SPLIT5
066200171011     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
066300171011     C                   EXSR      CHKNUM
066400171011     C                   IF        PiNum=*on
066500171011     C                   SELECT
066600171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'GRM'
066700171011     C                   EVAL      VABPKB = PiVal / 1000
066800171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'KGM'
066900171011     C                   EVAL      VABPKB = PiVal
067000171011     C                   OTHER
067100171011     C                   EVAL      VABPKB = PiVal
067200171011     C                   ENDSL
067300171011     C                   ELSE
067400171011     C                   Z-ADD     1             wRecErr
067500171011     C                   ENDIF
067600171011     C                   ENDIF
067700171011     C*
067800171011     C***
067900171011     C* ...tipo record 'MEA+AAE+AAW+' (volume spedizione)
068000171011     C                   EVAL      wTag = 'MEA+AAE+AAW'
068100171011     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068200171011     C                   EVAL      InStringa =
068300171011     C                                %subst(wvindta:%len(%trim(wTag))+2)
068400171011     C                   EVAL      InSepara = ':'
068500171011     C                   EXSR      SR_SPLIT5
068600171011     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
068700171011     C                   EXSR      CHKNUM
068800171011     C                   IF        PiNum=*on
068900171011     C                   SELECT
069000171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'LTR'
069100171011     C                   EVAL      VABVLB = PiVal / 1000
069200171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'HLT'
069300171011     C                   EVAL      VABVLB = PiVal / 10
069400171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'CMQ'
069500171011     C                   EVAL      VABVLB = PiVal / 1000000
069600171011     C                   WHEN      %trim(SkSplitCSV_5(1)) = 'MTQ'
069700171011     C                   EVAL      VABVLB = PiVal
069800171011     C                   OTHER
069900171011     C                   EVAL      VABVLB = PiVal
070000171011     C                   ENDSL
070100171011     C                   ELSE
070200171011     C                   Z-ADD     1             wRecErr
070300171011     C                   ENDIF
070400171011     C                   ENDIF
070500161025     C*
070600161025     C***
070700171011     C* ...tipo record 'GID' (numero colli)
070800171011     C                   EVAL      wTag = 'GID'
070900170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071000170209     C                   EVAL      InStringa =
071100170209     C                                %subst(wvindta:%len(%trim(wTag))+2)
071200171011     C                   EVAL      InSepara = '+'
071300171011     C                   EXSR      SR_SPLIT5
071400171011     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
071500171011     C                   EVAL      InSepara = ':'
071600171011     C                   EXSR      SR_SPLIT4
071700171011     C                   EVAL      PiStr=%trim(SkSplitCSV_4(1))
071800170209     C                   EXSR      CHKNUM
071900170209     C                   IF        PiInt=*on
072000171011     C                   Z-ADD     PiVal         VABNCL
072100170209     C                   ELSE
072200170209     C                   Z-ADD     1             wRecErr
072300170209     C                   ENDIF
072400170209     C                   ENDIF
072500141028     C*
072600141028     C***
072700170208     C* ...tipo record 'NAD+DP' (destinatario)
072800171011     C                   EVAL      wTag = 'NAD+DP'
072900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073000141028     C                   EVAL      InStringa =
073100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
073200141028     C                   EVAL      InSepara = '+'
073300171011     C                   EXSR      SR_SPLIT5
073400171011     C*
073500171011     C                   EVAL      InStringa = %trim(SkSplitCSV_5(1))
073600171011     C                   EVAL      InSepara = ':'
073700171011     C                   EXSR      SR_SPLIT4
073800171011     C                   EVAL      VABRMA = %trim(SkSplitCSV_4(1))
073900171011     C                   EVAL      PiStr=%trim(SkSplitCSV_4(1))
074000171011     C                   EXSR      CHKNUM
074100171011     C                   IF        PiInt=*on
074200171011     C                   Z-ADD     PiVal         VABRMN
074300171011     C                   ENDIF
074400171011     C*
074500141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
074600141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
074700141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
074800141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
074900141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
075000141028     C                   IF        VABNZD = 'I'   OR
075100141028     C                             VABNZD = 'IT'  OR
075200141028     C                             VABNZD = 'ITA'
075300171011     C                   EVAL      VABNZD = *blanks
075400171011     C                   ELSE
075500171011     C                   Z-ADD     1             jNAZ
075600171011     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
075700171011     C                   IF        %found
075800171011     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
075900171011     C                   ENDIF
076000141028     C                   ENDIF
076100171011     C*
076200141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
076300141028     C                   EVAL      InSepara = ':'
076400171011     C                   EXSR      SR_SPLIT4
076500171011     C                   EVAL      VABRSD = %trim(SkSplitCSV_4(1))
076600171011     C                   EVAL      VABRD2 = %trim(SkSplitCSV_4(2))
076700171011     C                   EVAL      VABNOT = %trim(SkSplitCSV_4(3))+
076800171011     C                                      %trim(SkSplitCSV_4(4))+
076900171011     C                                      %trim(SkSplitCSV_4(5))
077000141028     C                   ENDIF
077100150609     C*
077200150609     C***
077300150609     C* ...tipo record 'NAD+CZ' (mittente originale)
077400150609     C                   EVAL      wTag = 'NAD+CZ'
077500150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077600150609     C                   EVAL      InStringa =
077700150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
077800150609     C                   EVAL      InSepara = '+'
077900171011     C                   EXSR      SR_SPLIT5
078000150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
078100150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
078200150609     C                   IF        VABNMO = 'I'   OR
078300150609     C                             VABNMO = 'IT'  OR
078400150609     C                             VABNMO = 'ITA'
078500150609     C                   EVAL      VABNMO = *blanks
078600161018     C                   ELSE
078700161018     C                   Z-ADD     1             jNAZ
078800161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
078900161018     C                   IF        %found
079000161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
079100161018     C                   ENDIF
079200161018     C                   ENDIF
079300161018     C*
079400150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
079500150609     C                   EVAL      InSepara = ':'
079600171011     C                   EXSR      SR_SPLIT4
079700171011     C                   EVAL      VABRMO = %trim(SkSplitCSV_4(1))
079800150609     C                   ENDIF
079900141028     C*
080000141028     C***
080100171011     C* ...tipo record 'PCI+IEN' (dettagli segnacolli)
080200171011     C                   EVAL      wTag = 'PCI+IEN'
080300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080400150526     C                   EVAL      InStringa =
080500150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
080600161014     C     '+':':'       XLATE     InStringa     InStringa
080700161013     C                   EVAL      InSepara = ':'
080800171011     C                   EXSR      SR_SPLIT5
080900150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
081000171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(1))
081100141028     C                   EVAL      VATTRC = 'E'
081200141028     C                   EXSR      WRIVAT
081300150526     C                   ENDIF
081400150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
081500171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(2))
081600150526     C                   EVAL      VATTRC = 'E'
081700150526     C                   EXSR      WRIVAT
081800150526     C                   ENDIF
081900150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
082000171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(3))
082100150526     C                   EVAL      VATTRC = 'E'
082200150526     C                   EXSR      WRIVAT
082300150526     C                   ENDIF
082400150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
082500171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(4))
082600150526     C                   EVAL      VATTRC = 'E'
082700150526     C                   EXSR      WRIVAT
082800150526     C                   ENDIF
082900150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
083000171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(5))
083100150526     C                   EVAL      VATTRC = 'E'
083200150526     C                   EXSR      WRIVAT
083300150526     C                   ENDIF
083400161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
083500171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(6))
083600161017     C                   EVAL      VATTRC = 'E'
083700161017     C                   EXSR      WRIVAT
083800161017     C                   ENDIF
083900161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
084000171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(7))
084100161017     C                   EVAL      VATTRC = 'E'
084200161017     C                   EXSR      WRIVAT
084300161017     C                   ENDIF
084400161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
084500171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(8))
084600161017     C                   EVAL      VATTRC = 'E'
084700161017     C                   EXSR      WRIVAT
084800161017     C                   ENDIF
084900161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
085000171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(9))
085100161017     C                   EVAL      VATTRC = 'E'
085200161017     C                   EXSR      WRIVAT
085300161017     C                   ENDIF
085400161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
085500171011     C                   EVAL      VATNOT = '00000000'+%trim(SkSplitCSV_5(10))
085600161017     C                   EVAL      VATTRC = 'E'
085700161017     C                   EXSR      WRIVAT
085800161017     C                   ENDIF
085900141028     C                   ENDIF
086000141028     C*
086100141028     C***
086200141028     C* ...tipo record 'UNT' (fine bolla)
086300141028     C                   EVAL      wTag = 'UNT'
086400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086500141028     C  N31              ADD       1             §CTROKVB
086600141028     C  N31              exsr      CHKWRI
086700140117     C  N31              WRITE     EDIVAB00
086800150526     C                   ENDIF
086900010202     C*
087000000801     C* Ebbene...
087100000801     C                   ADD       1             §CTRMO
087200141028     C                   IF        wRecErr <> *zeros
087300000801     C                   ADD       1             §CTRNO
087400000801     C                   EVAL      vinflg = '2'
087500000801     C                   ENDIF
087600000801     C*
087700000801     C                   ENDSR
087800010202     C*----------------------------------------------------*
087900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
088000010202     C*----------------------------------------------------*
088100020305     C     PREVAT        BEGSR
088200010202     C*
088300140117     C* Compongo il nome del membro da dare al EDIVATWR
088400010202     C                   eval      parmbr = vlrhdl
088500010202     C                   movel     'M'           parmbr
088600060113     C                   eval      parccm = vlrksc
088700010202     C                   eval      paropz = '1'
088800010202     C* Effettuo la chiamata al CLLE preposto
088900140117     C                   call(e)   'TITVEVTC'
089000010202     C                   parm                    parccm
089100010202     C                   parm                    parmbr
089200010202     C                   parm                    paropz
089300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
089400010202     C                   if        %error
089500010202     C                   movel     '1'           chkcall
089600010202     C                   else
089700010202     C                   movel     '0'           chkcall
089800010202     C                   endif
089900010202     C*
090000010202     C                   ENDSR
090100141028     C*----------------------------------------------------*
090200141028     C*  CONTROLLO NUMERICITA' CAMPI
090300141028     C*----------------------------------------------------*
090400141028     C     CHKNUM        BEGSR
090500141028     C*
090600141028     C                   IF        PiDecChr = *blanks
090700141028     C                   EVAL      PiDecChr = CharNUM
090800141028     C                   ENDIF
090900141028     C*
091000141028     C                   callp     UBISNUM_Check(PiStr
091100141028     C                                          :PiDecChr
091200141028     C                                          :PiVal
091300141028     C                                          :PiNum
091400141028     C                                          :PiInt)
091500141028     C*
091600141028     C                   ENDSR
091700141028     C***
091800141028
091900141028
092000141028     C*----------------------------------------------------*
092100171011     C*  SPLITTAMENTO DATI - "TIPO 4"
092200141028     C*----------------------------------------------------*
092300171011     C     SR_SPLIT4     BEGSR
092400141028     C*
092500141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
092600141028     C                   CALL      'XSPLIT2'
092700141028     C                   PARM                    InStringa
092800141028     C                   PARM                    InSepara
092900171011     C                   PARM      '4'           InTypeOut
093000141028     C                   PARM      ''            wSkParam
093100141028     C                   PARM                    OutErrore
093200141028     C                   PARM                    OutMsg
093300171011     C                   MOVEA     wSkParam      SkSplitCSV_4
093400141028     C*
093500141028     C                   ENDSR
093600171011
093700171011
093800171011     C*----------------------------------------------------*
093900171011     C*  SPLITTAMENTO DATI - "TIPO 5"
094000171011     C*----------------------------------------------------*
094100171011     C     SR_SPLIT5     BEGSR
094200171011     C*
094300171011     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
094400171011     C                   CALL      'XSPLIT2'
094500171011     C                   PARM                    InStringa
094600171011     C                   PARM                    InSepara
094700171011     C                   PARM      '5'           InTypeOut
094800171011     C                   PARM      ''            wSkParam
094900171011     C                   PARM                    OutErrore
095000171011     C                   PARM                    OutMsg
095100171011     C                   MOVEA     wSkParam      SkSplitCSV_5
095200171011     C*
095300171011     C                   ENDSR
095400141028
095500141028
095600141028     C*----------------------------------------------------*
095700141028     C*  SCRITTURA BUFFER EDIVAT
095800141028     C*----------------------------------------------------*
095900141028     C     WRIVAT        BEGSR
096000141028     C*
096100141028     C                   IF        VATNOT <> *blanks
096200141028     C                   EVAL      VATFGS = VABFGS
096300141028     C                   EVAL      VATCCM = VABCCM
096400141028     C                   EVAL      VATLNP = VABLNP
096500141028     C                   EVAL      VATAAS = VABAAS
096600141028     C                   EVAL      VATNRS = VABNRS
096700141028     C                   EVAL      VATNSP = VABNSP
096800141028     C                   EVAL      VATCMR = VABCMR
096900141028     C                   EVAL      VATCNT = VABCNT
097000141028     C                   ADD       1             §CTROKVT
097100141028     C                   WRITE     EDIVAT00
097200141028     C                   ENDIF
097300141028     C*
097400141028     C                   ENDSR
097500000801
097600011113
097700011113     C*----------------------------------------------------*
097800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
097900011113     C*----------------------------------------------------*
098000011113     C     CHKIMPDIV     BEGSR
098100011113     C*
098200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
098300011113     C                   Z-ADD     *zeros        wrkDec            9 9
098400011113     C*
098500011113     C* Come prima cosa effettuo considerazioni sulla divisa
098600011113     C                   IF        vabIAS > *zeros
098700011113     C                   IF        vabVAS <> 'EUR'
098800011113     C                   EVAL      vabVAS =  'ITL'
098900011113     C                   ENDIF
099000011113     C                   ENDIF
099100011113     C*
099200011113     C                   IF        vabCAS > *zeros
099300011113     C                   IF        vabVCA <> 'EUR'
099400011113     C                   EVAL      vabVCA =  'ITL'
099500011113     C                   ENDIF
099600011113     C                   ENDIF
099700011113     C*
099800011113     C                   IF        vabVMD > *zeros
099900020305     C                   IF        vabVAD <> 'EUR'
100000011113     C                   EVAL      vabVAD =  'ITL'
100100011113     C                   ENDIF
100200011113     C                   ENDIF
100300011113     C*
100400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
100500011113     C                   Z-ADD     vabIAS        wrkDec
100600011113     C                   IF        wrkDec > *zeros
100700011113     C                   IF        vabVAS = 'ITL'
100800011113     C                   EVAL      vabIAS = *zeros
100900011113     C                   ENDIF
101000011113     C                   ENDIF
101100011113     C*
101200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
101300011113     C                   Z-ADD     vabCAS        wrkDec
101400011113     C                   IF        wrkDec > *zeros
101500011113     C                   IF        vabVCA = 'ITL'
101600011113     C                   EVAL      vabCAS = *zeros
101700011113     C                   ENDIF
101800011113     C                   ENDIF
101900011113     C*
102000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
102100011113     C                   Z-ADD     vabVMD        wrkDec
102200011113     C                   IF        wrkDec > *zeros
102300011113     C                   IF        vabVAD = 'ITL'
102400011113     C                   EVAL      vabVMD = *zeros
102500011113     C                   ENDIF
102600011113     C                   ENDIF
102700011113     C*
102800011113     C                   ENDSR
102900011113     C***
103000011113
103100011113
103200000801
103300000801
103400990920      /TITLE Invio dei dati al punto operativo.
103500010202     C     invio         BEGSR
103600990920     C*
103700140117     C* 1° invio EDIVAT
103800010201     C                   reset                   dscmz
103900010201     C                   move      vlrpoi        cmzdst
104000140117     C                   eval      cmzfld = 'EDIVATWR'
104100010201     C                   eval      cmzmbd = vlrhdl
104200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
104300021009     C***                if        prmfir = *blanks
104400140117     C                   eval      cmzfla = 'EDIVAT0F'
104500140117     C                   eval      cmzmba = 'EDIVAT0F'
104600021009     C***                else
104700021009     C***                eval      cmzfla = prmfir
104800021009     C***                eval      cmzmba = prmfir
104900021009     C***                endif
105000010201     C                   eval      cmznrr = *zeros
105100020305     C                   move      §ctrokvt      cmznrr
105200021018     C                   eval      cmzlba = vlrfl1
105300010201     C                   call(e)   'TIS711C'
105400010201     C                   parm                    dscmz
105500010201     C                   parm      *blanks       esito
105600010205     C                   if        %error
105700010205     C                             or cmzerr = '1'
105800010205     C                             or esito  = '1'
105900010205     C                   eval      wrkesito = '3'
106000010205     C                   else
106100010201     C*
106200140117     C* 2° invio EDIVAB
106300010201     C                   reset                   dscmz
106400010201     C                   move      vlrpoi        cmzdst
106500010201     C                   eval      cmzfld = vlrfou
106600010201     C                   eval      cmzmbd = vlrhdl
106700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
106800021009     C***                if        prmfir = *blanks
106900140117     C                   eval      cmzfla = 'EDIVAB0F'
107000140117     C                   eval      cmzmba = 'EDIVAB0F'
107100021009     C***                else
107200021009     C***                eval      cmzfla = prmfir
107300021009     C***                eval      cmzmba = prmfir
107400021009     C***                endif
107500010201     C                   eval      cmznrr = *zeros
107600010201     C                   move      §ctrokvb      cmznrr
107700021018     C                   eval      cmzlba = vlrfl1
107800010201     C                   call(e)   'TIS711C'
107900010201     C                   parm                    dscmz
108000010201     C                   parm      *blanks       esito
108100010201     C                   if        %error
108200010201     C                             or cmzerr = '1'
108300010201     C                             or esito  = '1'
108400010201     C                   eval      wrkesito = '3'
108500010201     C                   endif
108600010205     C                   endif
108700990920     C*
108800000613     C                   ENDSR
108900000613     C***
109000070411
109100141028
109200070411     C     *pssr         BEGSR
109300070411     C*
109400070411     C                   if        %open(tivin00r)
109500070411     C                   close     tivin00r
109600070411     C                   endif
109700140117     C                   if        %open(edivabwr)
109800140117     C                   close     edivabwr
109900070411     C                   endif
110000140117     C                   if        %open(edivatwr)
110100140117     C                   close     edivatwr
110200070411     C                   endif
110300070411     C*
110400070411     C* Effettuo la chiamata al CLLE preposto
110500140117     C                   call(e)   'TITVEVTC'
110600070411     C                   parm                    parccm
110700070411     C                   parm                    parmbr
110800070411     C                   parm      '2'           paropz
110900070411     C*
111000070411     C                   eval      wrkesito = '2'
111100070411     C*
111200070411     C                   seton                                        LR
111300070411     C*
111400070411     C                   ENDSR     '*CANCL'
111500070411     C***
111600161018
111700161018
111800161018     C*--------------------------------------------------------
111900161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
112000161018     C*--------------------------------------------------------
112100161018     C     CARTAB        BEGSR
112200161018     C*
112300161018     C* TABELLA '15' - NAZIONI
112400161018     C                   clear                   skNAZISO
112500161018     C                   clear                   skNAZBAR
112600161018     C                   eval      tblKUT = 1
112700161018     C                   eval      tblCOD = '15'
112800161018     C     KEYtabP       setll     tabel00f
112900161018     C     KEYtabP       reade     tabel00f
113000161018     C                   dow       not %eof(tabel00f)
113100161018     C                   if        tblFLG = *blanks
113200161018     C                   movel(p)  tblUNI        ds15
113300161018     C                   add       1             jNAZ
113400161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
113500161018     C                   eval      skNAZISO(jNAZ) = §15COD
113600161018     C                   endif
113700161018     C     KEYtabP       reade     tabel00f
113800161018     C                   enddo
113900161018     C*
114000161018     C                   ENDSR
114100161018     C***
114200070411
114300990910
114400000613     C     *inzsr        BEGSR
114500990910     C*
114600990910     C     *entry        plist
114700990920     C                   parm                    tivlrds
114800990921     C                   parm      wrkesito      esito
114900000724     C                   parm                    prmlit
115000000710     C                   parm                    prmfir
115100000613     C*
115200000830     C* CALCOLA LA DATA CORRENTE
115300140117     C                   time                    wn14             14 0
115400140117     C                   movel     wn14          oracor            6 0          *ORA
115500100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
115600100722     C                   eval      datcor = %dec(%date() : *ISO)
115700161018     C*
115800161018     C* Chiave su TABEL00F - parziale
115900161018     C     KEYtabP       KLIST
116000161018     C                   KFLD                    tblKUT
116100161018     C                   KFLD                    tblCOD
116200000830     C*
116300000613     C                   ENDSR
116400000613     C***
