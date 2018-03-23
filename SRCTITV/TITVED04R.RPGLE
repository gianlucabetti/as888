000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400170221     Ftabel00f  if   e           k disk
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
001700140117     D edivabds      e ds                  extname(edivab0f)
001800170221     D ds15          e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800141028     D wTag            s            128    VARYING INZ
002900141028     D wRecErr         s              1  0 INZ(*zeros)
003000141028     D wFlgCAS         s              1    INZ(*blanks)
003100141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003200150527     D wNOTE           s             70    INZ(*blanks)
003300141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003900141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004000170221
004100170221     D*------------
004200170221     D jNAZ            s              5  0 INZ(*zeros)
004300170221     D skNAZISO        S              3    DIM(1000)
004400170221     D skNAZBAR        S              3    DIM(1000)
004500141028
004600141028     D*------------------
004700141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800141028     D*------------------
004900141028     D CharNUM         S              1
005000141028     D CharSOS         S              1
005100141028     D posDaDft        S              3  0 INZ(*zeros)
005200141028     D posADft         S              3  0 INZ(*zeros)
005300141028     D wGiroDft        s              1  0 INZ(*zeros)
005400141028
005500141028
005600141028     D*------------------
005700141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005800141028     D*------------------
005900141028     D InStringa       S          65535A   VARYING                              (stringa input)
006000141028     D InSepara        S             10A                                        (separatore)
006100141028     D InTypeOut       S              1A                                        (tipo output)
006200141028     D wSkParam        S          65535A                                        (output)
006300141028     D OutErrore       S              1A                                        (flag errore)
006400141028     D OutMsg          S             80A                                        (messaggio errore)
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
009700150526     C                   setoff                                       90
009800000613     C*
009900170221     C                   EXSR      CARTAB                                       CARICA TABELLE
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100070719     C*
010200070719     C* Esegue lancio TISI95R solo x chiusura
010300070719     C                   CLEAR                   TISI95DS
010400070719     C                   EVAL      I95TLA = 'C'
010500070719     C                   CALL      'TISI95R'
010600070719     C                   PARM                    TISI95DS
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900140922     C                   call(e)   'TITVEVTC'
011000140922     C                   parm                    parccm
011100140922     C                   parm                    parmbr
011200140922     C                   parm      '2'           paropz
011300170731     C*
011400170731     C                   if        *in53
011500170731     C                   call      'TIS7P2SER'
011600170731     C                   parm      'C'           pIn_Opz           1
011700170731     C                   parm                    tivlrds
011800170731     C                   parm                    EDIVABDS
011900170731     C                   parm      *blanks       pIn_CdIdAz        2
012000170731     C                   parm      *blanks       pIn_MaskPDF      50
012100170731     C                   parm      *blanks       pOut_Esito        1
012200170731     C                   endif
012300000616     C*
012400010201     C                   seton                                        LR
012500990908
012600000801
012700910830     C*--------------------------------------------------------
012800140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012900910830     C*--------------------------------------------------------
013000040526     C     RWFILE        BEGSR
013100990910     C*
013200990914     C                   if        not %open(tivin00r)
013300990908     C                   open      tivin00r
013400990914     C                   endif
013500140117     C                   if        not %open(edivabwr)
013600140117     C                   open      edivabwr
013700990914     C                   endif
013800141028     C*
013900140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014000020305     C                   exsr      prevat
014100010201     C*
014200010202     C                   if        chkcall = '0'
014300010202     C*
014400140117     C                   if        not %open(edivatwr)
014500140117     C                   open      edivatwr
014600010201     C                   endif
014700990910     C*
014800010201     C                   clear                   §CTROKVB          5 0
014900020305     C                   clear                   §CTROKVT          5 0
015000000801     C                   clear                   §CTRMO            5 0
015100000801     C                   clear                   §CTRNO            5 0
015200141028     C*
015300141028     C* Inizializzazioni fuori ciclo
015400141028     C                   EXSR      INZVAR
015500140117     C*
015600140902     C                   if        *in53
015700140117     C                   call      'TIS7P2SER'
015800140117     C                   parm      'O'           pIn_Opz           1
015900140117     C                   parm                    tivlrds
016000140117     C                   parm                    EDIVABDS
016100140117     C                   parm      *blanks       pIn_CdIdAz        2
016200140117     C                   parm      *blanks       pIn_MaskPDF      50
016300140117     C                   parm      *blanks       pOut_Esito        1
016400140902     C                   endif
016500130708     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900040910     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900141028     C*
018000141028     C* "normalizzo" il dato di input
018100141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
018200141028     C                                             %len(%trim(vindta))-1))
018300160209     C*
018400160209     C* Elimino le "duple di controllo EDIFACT"
018500160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
018600160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018700160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018800160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018900160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
019000040910     C*
019100040910     C* Eseguo routine d traduzione
019200040910     C                   exsr      impvabvat
019300040802     C*
019400010305     C                   endif
019500000905     C*
019600000905     C                   else
019700000905     C                   eval      vinflg = '1'
019800000905     C                   endif
019900000905     C*
020000000905     C  N70              update    tivin000
020100000905     C*
020200991022     C  N70              ENDdo
020300100722     C*
020400100722     C* Scarico i buffer testata ancora "in canna"
020500140922     C*
020600141028     C***  N31              ADD       1             §CTROKVB
020700141028     C***  N31              exsr      CHKWRI
020800141028     C***  N31              WRITE     EDIVAB00
020900010202     C*
021000010202     C                   endif
021100990910
021200990910     C* Se non ci sono record con errori ...
021300000710     C                   if        §ctrno = 0
021400990910     C* ... restituisco esito OK.
021500990921     C                   eval      wrkesito = '0'
021600990910     C                   else
021700010201     C                   if        §ctrokvb > 0
021800990921     C                   eval      wrkesito = '1'
021900000710     C                   else
022000000710     C                   eval      wrkesito = '2'
022100990910     C                   endif
022200000710     C                   endif
022300990910     C*
022400990914     C                   if        %open(tivin00r)
022500990908     C                   close     tivin00r
022600990914     C                   endif
022700140117     C                   if        %open(edivabwr)
022800140117     C                   close     edivabwr
022900990914     C                   endif
023000140117     C                   if        %open(edivatwr)
023100140117     C                   close     edivatwr
023200010201     C                   endif
023300990910     C*
023400010201     C                   if        §ctrokvb > 0
023500000724     C                             and vlrpoi <> *zeros
023600010202     C                   exsr      invio
023700990920     C                   endif
023800990920     C*
023900910830     C                   ENDSR
024000000613     C***
024100140922
024200140922     C*----------------------------------------------------*
024300140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
024400140922     C*----------------------------------------------------*
024500140922     C     CHKWRI        BEGSR
024600140922     C*
024700141028     C                   if        VABRMN = *zeros
024800141028     C                   eval      VABRMN = VABNSP
024900141028     C                   endif
025000141028     C*
025100141028     C                   if        VABPRD = *blanks
025200141028     C* ......VABPRD
025300141028     C* Reperisco la provincia dal CAP e dalla località
025400141028     C                   IF        VABLOD <> *blanks AND
025500141028     C                             VABCAD <> *blanks AND
025600141028     C                             VABNZD  = *blanks
025700141028     C                   CLEAR                   TISI95DS
025800141028     C                   EVAL      I95TCN = '3'
025900141028     C                   Z-ADD     datcor        I95DAT
026000141028     C                   EVAL      I95CAP = VABCAD
026100141028     C                   EVAL      I95LOC = VABLOD
026200141028     C                   CALL      'TISI95R'
026300141028     C                   PARM                    TISI95DS
026400141028     C                   EVAL      VABPRD = O95PRV
026500141028     C                   ENDIF
026600141028     C                   endif
026700140922     C*
026800140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
026900141028     C                   IF        wFlgCAS <> '0'
027000140922     C                   IF        VABCBO = '1'
027100140922     C                   EVAL      VABCBO = '4'
027200140922     C                   ELSE
027300140922     C                   EVAL      VABCBO = '6'
027400140922     C                   ENDIF
027500140922     C                   ENDIF
027600140922     C*
027700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027800141028     C                   EXSR      CHKIMPDIV
027900150527     C*
028000150527     C* Scompongo le note
028100150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
028200150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
028300140922     C*
028400140922     C                   ENDSR
028500141028     C*----------------------------------------------------*
028600141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700141028     C*----------------------------------------------------*
028800141028     C     INZVAR        BEGSR
028900141028     C*
029000141028     C* Inizializzo variabili di wrk
029100141028     C                   Z-ADD     *zeros        Num5_0            5 0
029200141028     C                   MOVEL     '0'           wFlgCAS
029300141028     C                   MOVEL     *blanks       wVATNOT_A
029400141028     C                   MOVEL     *blanks       wVATNOT_B
029500141028     C                   MOVEL     *blanks       wVATNOT_E
029600141028     C                   MOVEL     *blanks       wVATNOT_I
029700141028     C                   MOVEL     *blanks       wVATNOT_J
029800141028     C                   MOVEL     *blanks       wVATNOT_S
029900141028     C                   MOVEL     *blanks       wVATNOT_P
030000141028     C                   CLEAR                   SkSplitCSV_5
030100150527     C                   MOVEL     *blanks       wNOTE
030200141028     C*
030300141028     C* Reimposto i valori di default
030400141028     C                   EXSR      DEFCAM
030500141028     C*
030600141028     C                   ENDSR
030700141028     C*----------------------------------------------------*
030800141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
030900141028     C*----------------------------------------------------*
031000141028     C     DEFCAM        BEGSR
031100141028     C*
031200141028     C                   CLEAR                   EDIVAB00
031300141028     C                   CLEAR                   EDIVAT00
031400141028     C*
031500141028     C* Imposto i valori bolla di default
031600141028     C                   EVAL      VABCTR = 000
031700141028     C                   EVAL      VABCBO = '1'
031800141104     C                   EVAL      VABTSP = 'C'
031900141028     C*
032000141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032100141028     C* e delimitatore testo.
032200141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032300141028     C*
032400141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
032500141028     C                   EVAL      CharSOS = CharNUM
032600141028     C*
032700141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
032800141028     C                   SETOFF                                       53
032900141028     C                   SELECT
033000141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
033100141028     C                   SETON                                        53
033200141028     C                   ENDSL
033300141028     C*
033400141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
033500141028     C                   EVAL      posDaDft = 1
033600141028     C                   EVAL      posADft  = 0
033700141028     C                   EVAL      wGiroDft = 0
033800141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
033900141028     C                             posDaDft > 0
034000141028     C*
034100141028     C* Gestisco il 1° giro
034200141028     C                   IF        wGiroDft = 0
034300141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
034400141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
034500141028     C* Incremento il contatore dei "giri"
034600141028     C                   EVAL      wGiroDft = 1
034700141028     C                   ELSE
034800141028     C                   EVAL      posDaDft = posADft
034900141028     C                   ENDIF
035000141028     C* Eseguo lo scan x trovare la fine del campo corrente
035100141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035200141028     C*
035300141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
035400141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
035500141028     C                   IF        posDaDft > 0 AND
035600141028     C                             posADft  > 0
035700141028     C* NCL
035800141028     C                   IF        %subst(
035900141028     C                             %subst(vlrppt:posDaDft+1:
036000141028     C                             posADft-posDaDft-1):1:3)
036100141028     C                             = 'NCL'
036200141028     C                   EVAL      PiStr=%trim(%subst(
036300141028     C                             %subst(vlrppt:posDaDft+1:
036400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500141028     C                   EXSR      CHKNUM
036600141028     C                   IF        PiInt=*on
036700141028     C                   Z-ADD     PiVal         VABNCL
036800141028     C                   ENDIF
036900141028     C                   ENDIF
037000141028     C* CCM
037100141028     C                   IF        %subst(
037200141028     C                             %subst(vlrppt:posDaDft+1:
037300141028     C                             posADft-posDaDft-1):1:3)
037400141028     C                             = 'CCM'
037500141028     C                   EVAL      PiStr=%trim(%subst(
037600141028     C                             %subst(vlrppt:posDaDft+1:
037700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800141028     C                   EXSR      CHKNUM
037900141028     C                   IF        PiInt=*on
038000141028     C                   Z-ADD     PiVal         VABCCM
038100141028     C                   ENDIF
038200141028     C                   ENDIF
038300141028     C* LNP
038400141028     C                   IF        %subst(
038500141028     C                             %subst(vlrppt:posDaDft+1:
038600141028     C                             posADft-posDaDft-1):1:3)
038700141028     C                             = 'LNP'
038800141028     C                   EVAL      PiStr=%trim(%subst(
038900141028     C                             %subst(vlrppt:posDaDft+1:
039000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100141028     C                   EXSR      CHKNUM
039200141028     C                   IF        PiInt=*on
039300141028     C                   Z-ADD     PiVal         VABLNP
039400141028     C                   ENDIF
039500141028     C                   ENDIF
039600141028     C* NRS
039700141028     C                   IF        %subst(
039800141028     C                             %subst(vlrppt:posDaDft+1:
039900141028     C                             posADft-posDaDft-1):1:3)
040000141028     C                             = 'NRS'
040100141028     C                   EVAL      PiStr=%trim(%subst(
040200141028     C                             %subst(vlrppt:posDaDft+1:
040300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040400141028     C                   EXSR      CHKNUM
040500141028     C                   IF        PiInt=*on
040600141028     C                   Z-ADD     PiVal         VABNRS
040700141028     C                   ENDIF
040800141028     C                   ENDIF
040900141028     C* CTR
041000141028     C                   IF        %subst(
041100141028     C                             %subst(vlrppt:posDaDft+1:
041200141028     C                             posADft-posDaDft-1):1:3)
041300141028     C                             = 'CTR'
041400141028     C                   EVAL      PiStr=%trim(%subst(
041500141028     C                             %subst(vlrppt:posDaDft+1:
041600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041700141028     C                   EXSR      CHKNUM
041800141028     C                   IF        PiInt=*on
041900141028     C                   Z-ADD     PiVal         VABCTR
042000141028     C                   ENDIF
042100141028     C                   ENDIF
042200141028     C* PKB
042300141028     C                   IF        %subst(
042400141028     C                             %subst(vlrppt:posDaDft+1:
042500141028     C                             posADft-posDaDft-1):1:3)
042600141028     C                             = 'PKB'
042700141028     C                   EVAL      PiStr=%trim(%subst(
042800141028     C                             %subst(vlrppt:posDaDft+1:
042900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043000141028     C                   EXSR      CHKNUM
043100141028     C                   IF        PiNum=*on
043200141028     C                   Z-ADD     PiVal         VABPKB
043300141028     C                   ENDIF
043400141028     C                   ENDIF
043500141028     C* VLB
043600141028     C                   IF        %subst(
043700141028     C                             %subst(vlrppt:posDaDft+1:
043800141028     C                             posADft-posDaDft-1):1:3)
043900141028     C                             = 'VLB'
044000141028     C                   EVAL      PiStr=%trim(%subst(
044100141028     C                             %subst(vlrppt:posDaDft+1:
044200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044300141028     C                   EXSR      CHKNUM
044400141028     C                   IF        PiNum=*on
044500141028     C                   Z-ADD     PiVal         VABVLB
044600141028     C                   ENDIF
044700141028     C                   ENDIF
044800141028     C* QFT
044900141028     C                   IF        %subst(
045000141028     C                             %subst(vlrppt:posDaDft+1:
045100141028     C                             posADft-posDaDft-1):1:3)
045200141028     C                             = 'QFT'
045300141028     C                   EVAL      PiStr=%trim(%subst(
045400141028     C                             %subst(vlrppt:posDaDft+1:
045500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045600141028     C                   EXSR      CHKNUM
045700141028     C                   IF        PiNum=*on
045800141028     C                   Z-ADD     PiVal         VABQFT
045900141028     C                   ENDIF
046000141028     C                   ENDIF
046100141028     C* CBO
046200141028     C                   IF        %subst(
046300141028     C                             %subst(vlrppt:posDaDft+1:
046400141028     C                             posADft-posDaDft-1):1:3)
046500141028     C                             = 'CBO'
046600141028     C                   EVAL      VABCBO=%trim(%subst(
046700141028     C                             %subst(vlrppt:posDaDft+1:
046800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046900141028     C                   ENDIF
047000141028     C* TSP
047100141028     C                   IF        %subst(
047200141028     C                             %subst(vlrppt:posDaDft+1:
047300141028     C                             posADft-posDaDft-1):1:3)
047400141028     C                             = 'TSP'
047500141028     C                   EVAL      VABTSP=%trim(%subst(
047600141028     C                             %subst(vlrppt:posDaDft+1:
047700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047800141028     C                   ENDIF
047900141028     C* VAS
048000141028     C                   IF        %subst(
048100141028     C                             %subst(vlrppt:posDaDft+1:
048200141028     C                             posADft-posDaDft-1):1:3)
048300141028     C                             = 'VAS'
048400141028     C                   EVAL      VABVAS=%trim(%subst(
048500141028     C                             %subst(vlrppt:posDaDft+1:
048600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048700141028     C                   ENDIF
048800141028     C* VCA
048900141028     C                   IF        %subst(
049000141028     C                             %subst(vlrppt:posDaDft+1:
049100141028     C                             posADft-posDaDft-1):1:3)
049200141028     C                             = 'VCA'
049300141028     C                   EVAL      VABVCA=%trim(%subst(
049400141028     C                             %subst(vlrppt:posDaDft+1:
049500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049600141028     C                   ENDIF
049700141028     C* TIC
049800141028     C                   IF        %subst(
049900141028     C                             %subst(vlrppt:posDaDft+1:
050000141028     C                             posADft-posDaDft-1):1:3)
050100141028     C                             = 'TIC'
050200141028     C                   EVAL      VABTIC=%trim(%subst(
050300141028     C                             %subst(vlrppt:posDaDft+1:
050400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050500141028     C                   ENDIF
050600141028     C* GCA
050700141028     C                   IF        %subst(
050800141028     C                             %subst(vlrppt:posDaDft+1:
050900141028     C                             posADft-posDaDft-1):1:3)
051000141028     C                             = 'GCA'
051100141028     C                   EVAL      VABGCA=%trim(%subst(
051200141028     C                             %subst(vlrppt:posDaDft+1:
051300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400141028     C                   ENDIF
051500141028     C* CTM
051600141028     C                   IF        %subst(
051700141028     C                             %subst(vlrppt:posDaDft+1:
051800141028     C                             posADft-posDaDft-1):1:3)
051900141028     C                             = 'CTM'
052000141028     C                   EVAL      VABCTM=%trim(%subst(
052100141028     C                             %subst(vlrppt:posDaDft+1:
052200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300141028     C                   ENDIF
052400141028     C* FFD
052500141028     C                   IF        %subst(
052600141028     C                             %subst(vlrppt:posDaDft+1:
052700141028     C                             posADft-posDaDft-1):1:3)
052800141028     C                             = 'FFD'
052900141028     C                   EVAL      VABFFD=%trim(%subst(
053000141028     C                             %subst(vlrppt:posDaDft+1:
053100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200141028     C                   ENDIF
053300141028     C* VAD
053400141028     C                   IF        %subst(
053500141028     C                             %subst(vlrppt:posDaDft+1:
053600141028     C                             posADft-posDaDft-1):1:3)
053700141028     C                             = 'VAD'
053800141028     C                   EVAL      VABVAD=%trim(%subst(
053900141028     C                             %subst(vlrppt:posDaDft+1:
054000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100141028     C                   ENDIF
054200141028     C* GMA
054300141028     C                   IF        %subst(
054400141028     C                             %subst(vlrppt:posDaDft+1:
054500141028     C                             posADft-posDaDft-1):1:3)
054600141028     C                             = 'GMA'
054700141028     C                   EVAL      VABGMA=%trim(%subst(
054800141028     C                             %subst(vlrppt:posDaDft+1:
054900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000141028     C                   ENDIF
055100141028     C* GGA
055200141028     C                   IF        %subst(
055300141028     C                             %subst(vlrppt:posDaDft+1:
055400141028     C                             posADft-posDaDft-1):1:3)
055500141028     C                             = 'GGA'
055600141028     C                   EVAL      VABGGA=%trim(%subst(
055700141028     C                             %subst(vlrppt:posDaDft+1:
055800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900141028     C                   ENDIF
056000141028     C* GVA
056100141028     C                   IF        %subst(
056200141028     C                             %subst(vlrppt:posDaDft+1:
056300141028     C                             posADft-posDaDft-1):1:3)
056400141028     C                             = 'GVA'
056500141028     C                   EVAL      VABGVA=%trim(%subst(
056600141028     C                             %subst(vlrppt:posDaDft+1:
056700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800141028     C                   ENDIF
056900141028     C* TC1
057000141028     C                   IF        %subst(
057100141028     C                             %subst(vlrppt:posDaDft+1:
057200141028     C                             posADft-posDaDft-1):1:3)
057300141028     C                             = 'TC1'
057400141028     C                   EVAL      VABTC1=%trim(%subst(
057500141028     C                             %subst(vlrppt:posDaDft+1:
057600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700141028     C                   ENDIF
057800141028     C* TC2
057900141028     C                   IF        %subst(
058000141028     C                             %subst(vlrppt:posDaDft+1:
058100141028     C                             posADft-posDaDft-1):1:3)
058200141028     C                             = 'TC2'
058300141028     C                   EVAL      VABTC2=%trim(%subst(
058400141028     C                             %subst(vlrppt:posDaDft+1:
058500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600141028     C                   ENDIF
058700141028     C* VATTRC
058800141028     C                   IF        %subst(
058900141028     C                             %subst(vlrppt:posDaDft+1:
059000141028     C                             posADft-posDaDft-1):1:3)
059100141028     C                             = 'TRC'
059200141028     C                   EVAL      VATTRC=%trim(%subst(
059300141028     C                             %subst(vlrppt:posDaDft+1:
059400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500141028     C                   ENDIF
059600141028     C* ...
059700141028     C                   ENDIF
059800141028     C                   ENDDO
059900141028     C*
060000141028     C                   ENDSR
060100000801     C*----------------------------------------------------*
060200140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
060300000801     C*----------------------------------------------------*
060400040910     C     IMPVABVAT     BEGSR
060500040910     C*
060600040910     C* Traduzione relativa ai tipi record del file del cliente
060700040910     C*
060800071210     C*
060900071210     C***
061000141028     C* ...tipo record 'UNB' (info CMR)
061100141028     C                   EVAL      wTag = 'UNB'
061200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061300141028     C                   MOVEL     *blanks       savCMR           35
061400141028     C                   EVAL      InStringa =
061500141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
061600141028     C                   EVAL      InSepara = '+'
061700141028     C                   EXSR      SR_SPLIT
061800141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
061900140922     C                                      %char(datcor) + ' ' + %char(oracor)
062000130620     C                   ENDIF
062100141028     C*
062200141028     C***
062300150526     C* ...tipo record 'CNI' (rottuta di codice spedizione)
062400150526     C                   EVAL      wTag = 'CNI'
062500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062600150526     C*
062700150526     C* Scarico buffer spedizione elaborata
062800150526     C                   IF        *IN90
062900150526     C  N31              ADD       1             §CTROKVB
063000150526     C  N31              exsr      CHKWRI
063100150526     C  N31              WRITE     EDIVAB00
063200150526     C                   ENDIF
063300150526     C                   SETON                                        90
063400150526     C*
063500150526     C* Inizializzazioni per nuova spedizione
063600141028     C                   SETOFF                                       31
063700141028     C                   eval      vinflg = '1'
063800141028     C* ......inizializzazioni iniziali e formati record file Bartolini
063900141028     C                   EXSR      INZVAR
064000141028     C* ......valorizzazione campi fissi
064100141028     C                   MOVEL     datcor        VABAAS
064200141028     C                   MOVE      datcor        VABMGS
064300141028     C                   MOVE(P)   vlrpoi        VABFGS
064400141028     C                   EVAL      VABCMR = savCMR
064500141028     C                   EVAL      VABDCM = datcor
064600141028     C                   EVAL      VABDTS = datcor
064700141028     C                   EVAL      VABCNT = 1
064800141028     C* ......NSP => Stacco un numeratore da AZNUM
064900141028     C                   clear                   TRUL33DS
065000141028     C                   eval      I33OPE = *zeros
065100141028     C                   eval      I33CNU = 302
065200141028     C                   eval      I33NUM = 1
065300141028     C                   movel     TRUL33DS      KPJBU
065400141028     C                   call      'TRUL33R'
065500141028     C                   parm                    KPJBA
065600141028     C                   movel     KPJBU         TRUL33DS
065700141028     C                   if        O33ERR = *zeros
065800141028     C                   z-add     O33NRF        VABNSP
065900141028     C                   else
066000141028     C                   Z-ADD     1             wRecErr
066100141028     C                   SETON                                        31
066200141028     C                   EVAL      vinmsg = %trimr(vinmsg)
066300141028     C                             + ' ' + 'VABNSP VATNSP'
066400141028     C                   endif
066500141028     C                   ENDIF
066600141028     C*
066700141028     C***
066800150526     C* ...tipo record 'RFF+FF' (riferimento spedizione)
066900150526     C                   EVAL      wTag = 'RFF+FF'
067000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067100150526     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
067200150526     C                   EXSR      CHKNUM
067300150526     C                   IF        PiInt=*on
067400150526     C                   Z-ADD     PiVal         VABRMN
067500150526     C                   ELSE
067600150526     C                   EVAL      VABRMN = 1
067700150526     C                   Z-ADD     1             wRecErr
067800150526     C                   ENDIF
067900141028     C                   ENDIF
068000150526     C*
068100150526     C***
068200150526     C* ...tipo record 'RFF+CR' (riferimento spedizione)
068300150526     C                   EVAL      wTag = 'RFF+CR'
068400150526     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068500150526     C                   EVAL      VABRMA =
068600150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
068700150526     C                   ENDIF
068800150527     C*
068900150527     C***
069000150527     C* ...tipo record 'DTM+2' (data consegna richiesta)
069100150527     C                   EVAL      wTag = 'DTM+2'
069200150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069300150527     C                   EVAL      PiStr=%subst(
069400150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
069500150527     C                   EXSR      CHKNUM
069600150527     C                   IF        PiInt=*on
069700150527     C                   Z-ADD     PiVal         VABDCR
069800150527     C                   ELSE
069900150527     C                   Z-ADD     1             wRecErr
070000150527     C                   ENDIF
070100150527     C                   ENDIF
070200141028     C*
070300141028     C***
070400141028     C* ...tipo record 'NAD+CN' (destinatario)
070500141028     C                   EVAL      wTag = 'NAD+CN'
070600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070700141028     C                   EVAL      InStringa =
070800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
070900141028     C                   EVAL      InSepara = '+'
071000141028     C                   EXSR      SR_SPLIT
071100141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
071200141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
071300141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
071400141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
071500141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
071600141028     C                   IF        VABNZD = 'I'   OR
071700141028     C                             VABNZD = 'IT'  OR
071800141028     C                             VABNZD = 'ITA'
071900141028     C                   EVAL      VABNZD = *blanks
072000141028     C                   ENDIF
072100141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
072200141028     C                   EVAL      InSepara = ':'
072300141028     C                   EXSR      SR_SPLIT
072400141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
072500141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
072600141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
072700141028     C                                      %trim(SkSplitCSV_5(4))+
072800141028     C                                      %trim(SkSplitCSV_5(5))
072900141028     C                   ENDIF
073000150609     C*
073100150609     C***
073200150609     C* ...tipo record 'NAD+CZ' (mittente originale)
073300150609     C                   EVAL      wTag = 'NAD+CZ'
073400150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073500150609     C                   EVAL      InStringa =
073600150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
073700150609     C                   EVAL      InSepara = '+'
073800150609     C                   EXSR      SR_SPLIT
073900150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
074000150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
074100150609     C                   IF        VABNMO = 'I'   OR
074200150609     C                             VABNMO = 'IT'  OR
074300150609     C                             VABNMO = 'ITA'
074400150609     C                   EVAL      VABNMO = *blanks
074500170221     C                   ELSE
074600170221     C                   Z-ADD     1             jNAZ
074700170221     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
074800170221     C                   IF        %found
074900170221     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
075000170221     C                   ENDIF
075100150609     C                   ENDIF
075200150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
075300150609     C                   EVAL      InSepara = ':'
075400150609     C                   EXSR      SR_SPLIT
075500150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
075600150609     C                   ENDIF
075700141104     C*
075800141104     C***
075900141104     C* ...tipo record 'NAD+UC' (destinatario)
076000141104     C                   EVAL      wTag = 'NAD+UC'
076100141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076200141104     C                   EVAL      InStringa =
076300141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
076400141104     C                   EVAL      InSepara = '+'
076500141104     C                   EXSR      SR_SPLIT
076600141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
076700141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
076800141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
076900141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
077000141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
077100141104     C                   IF        VABNZD = 'I'   OR
077200141104     C                             VABNZD = 'IT'  OR
077300141104     C                             VABNZD = 'ITA'
077400141104     C                   EVAL      VABNZD = *blanks
077500141104     C                   ENDIF
077600141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
077700141104     C                   EVAL      InSepara = ':'
077800141104     C                   EXSR      SR_SPLIT
077900141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
078000141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
078100141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
078200141104     C                                      %trim(SkSplitCSV_5(4))+
078300141104     C                                      %trim(SkSplitCSV_5(5))
078400141104     C                   ENDIF
078500141028     C*
078600141028     C***
078700141028     C* ...tipo record 'GID' (colli)
078800150609     C                   EVAL      wTag = 'GID+'
078900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079000141028     C                   EVAL      InStringa =
079100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079200141028     C                   EVAL      InSepara = '+'
079300141028     C                   EXSR      SR_SPLIT
079400150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
079500150526     C                   EVAL      InSepara = ':'
079600150526     C                   EXSR      SR_SPLIT
079700150526     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
079800141028     C                   EXSR      CHKNUM
079900141028     C                   IF        PiInt=*on
080000150526     C                   ADD       PiVal         VABNCL
080100141028     C                   ELSE
080200141028     C                   Z-ADD     1             wRecErr
080300141028     C                   ENDIF
080400141028     C                   ENDIF
080500150527     C*
080600150527     C***
080700150527     C* ...tipo record 'MOA+AAA' (contrassegno / divisa)
080800150527     C                   EVAL      wTag = 'MOA+AAA'
080900150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081000150527     C                   EVAL      wFlgCAS = '1'
081100150527     C                   EVAL      InStringa =
081200150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
081300150527     C                   EVAL      InSepara = ':'
081400150527     C                   EXSR      SR_SPLIT
081500150527     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
081600150527     C                   EXSR      CHKNUM
081700150527     C                   IF        PiNum=*on
081800150527     C                   ADD       PiVal         VABCAS
081900150527     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
082000150527     C                   ELSE
082100150527     C                   Z-ADD     1             wRecErr
082200150527     C                   ENDIF
082300150527     C                   ENDIF
082400141028     C*
082500141028     C***
082600141028     C* ...tipo record 'FTX+AAA' (natura merce)
082700141028     C                   EVAL      wTag = 'FTX+AAA'
082800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082900141028     C                   EVAL      InStringa =
083000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
083100141028     C                   EVAL      InSepara = '+'
083200141028     C                   EXSR      SR_SPLIT
083300141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
083400141028     C                   ENDIF
083500150622     C*
083600150622     C***
083700150622     C* ...tipo record 'TSR+' (tipo servizio)
083800150622     C                   EVAL      wTag = 'TSR+'
083900150622     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
084000150622     C                   EVAL      InStringa =
084100150622     C                                %subst(wvindta:%len(%trim(wTag))+2)
084200150622     C                   EVAL      InSepara = '+'
084300150622     C                   EXSR      SR_SPLIT
084400150622     C                   SELECT
084500150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = '3'
084600150622     C                   EVAL      VABTSP = 'C'
084700150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'E'
084800150622     C                   EVAL      VABTSP = 'E'
084900150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'H'
085000150622     C                   EVAL      VABTSP = 'H'
085100150622     C                   ENDSL
085200150622     C                   ENDIF
085300150527     C*
085400150527     C***
085500150527     C* ...tipo record 'FTX+DIN' (note di consegna)
085600150527     C                   EVAL      wTag = 'FTX+DIN'
085700150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085800150527     C                   EVAL      InStringa =
085900150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
086000150527     C                   EVAL      InSepara = '+'
086100150527     C                   EXSR      SR_SPLIT
086200150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
086300150527     C                                     %trim(SkSplitCSV_5(3))
086400150527     C                   ENDIF
086500150527     C*
086600150527     C***
086700150527     C* ...tipo record 'FTX+ACB' (note aggiuntive)
086800150527     C                   EVAL      wTag = 'FTX+ACB'
086900150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087000150527     C                   EVAL      InStringa =
087100150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
087200150527     C                   EVAL      InSepara = '+'
087300150527     C                   EXSR      SR_SPLIT
087400150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
087500150527     C                                     %trim(SkSplitCSV_5(3))
087600150527     C                   ENDIF
087700150609     C*
087800150609     C***
087900160513     C* ...tipo record 'FTX+AAI' (telefono destinatario / note / appuntamento / etc...)
088000150609     C                   EVAL      wTag = 'FTX+AAI'
088100150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088200150609     C                   EVAL      InStringa =
088300150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
088400150609     C                   EVAL      InSepara = '+'
088500150609     C                   EXSR      SR_SPLIT
088600150609     C                   MOVEL     *blanks       wSplitCSV_5     256
088700150609     C                   EVAL      wSplitCSV_5 = SkSplitCSV_5(3)
088800150609     C     minu:maiu     XLATE     wSplitCSV_5   wSplitCSV_5                    *Minus -> Maiuscolo
088900160513     C                   SELECT
089000160513     C*
089100160513     C                   WHEN      %scan('PHONE':wSplitCSV_5) > *zeros
089200150609     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
089300150609     C                                            %scan('PHONE':wSplitCSV_5)+
089400150609     C                                            %len('PHONE')+1))
089500150609     C                   EVAL      VATTRC = 'B'
089600150609     C                   EXSR      WRIVAT
089700160513     C*
089800160513     C                   WHEN      %scan('TELF':wSplitCSV_5) > *zeros
089900160513     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
090000160513     C                                            %scan('TELF':wSplitCSV_5)+
090100160513     C                                            %len('TELF')+1))
090200160513     C                   EVAL      VATTRC = 'B'
090300160513     C                   EXSR      WRIVAT
090400160513     C*
090500160513     C                   WHEN      %scan('CHIAMARE PRIMA DELLA CONSEGNA':
090600160513     C                                   wSplitCSV_5) > *zeros
090700160513     C                   EVAL      VABTC1 = 'A'
090800160513     C*
090900160513     C                   WHEN      %scan('CONSEGNA TASSATIVA':
091000160513     C                                   wSplitCSV_5) > *zeros
091100160513     C                   Z-ADD     *zeros        wPos              3 0
091200160513     C                   MOVEL     *blanks       wDCR             10
091300160513     C                   EVAL      wPos = %scan('CONSEGNA TASSATIVA':
091400160513     C                                          wSplitCSV_5)
091500160513     C                   EVAL      wDCR   = %trim(%subst(wSplitCSV_5:wPos+1))
091600160513     C                   EVAL      PiStr=%subst(wDCR:7:4)+
091700160513     C                                   %subst(wDCR:4:2)+
091800160513     C                                   %subst(wDCR:1:2)
091900160513     C                   EXSR      CHKNUM
092000160513     C                   IF        PiNum=*on
092100160513     C                   ADD       PiVal         VABDCR
092200160513     C                   ELSE
092300160513     C                   Z-ADD     1             wRecErr
092400160513     C                   ENDIF
092500160513     C*
092600160513     C                   OTHER
092700150609     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
092800150609     C                                     %trim(SkSplitCSV_5(3))
092900160513     C                   ENDSL
093000150609     C                   ENDIF
093100141028     C*
093200141028     C***
093300141028     C* ...tipo record 'MEA+WT+AAE' (peso spedizione)
093400141028     C                   EVAL      wTag = 'MEA+WT+AAE'
093500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093600141028     C                   EVAL      InStringa =
093700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
093800141028     C                   EVAL      InSepara = ':'
093900141028     C                   EXSR      SR_SPLIT
094000141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
094100141028     C                   EXSR      CHKNUM
094200141028     C                   IF        PiNum=*on
094300150924     C                   ADD       PiVal         VABPKB
094400141028     C                   ELSE
094500141028     C                   Z-ADD     1             wRecErr
094600141028     C                   ENDIF
094700141028     C                   ENDIF
094800141028     C*
094900141028     C***
095000141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
095100141028     C                   EVAL      wTag = 'MEA+VOL'
095200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095300141028     C                   EVAL      InStringa =
095400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
095500141028     C                   EVAL      InSepara = ':'
095600141028     C                   EXSR      SR_SPLIT
095700141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
095800141028     C                   EXSR      CHKNUM
095900141028     C                   IF        PiNum=*on
096000150924     C                   ADD       PiVal         VABVLB
096100141028     C                   ELSE
096200141028     C                   Z-ADD     1             wRecErr
096300141028     C                   ENDIF
096400141028     C                   ENDIF
096500141028     C*
096600141028     C***
096700150526     C* ...tipo record 'GIN+002' (dettagli segnacolli)
096800150526     C                   EVAL      wTag = 'GIN+002'
096900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097000150526     C                   EVAL      InStringa =
097100150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
097200150526     C                   EVAL      InSepara = '+'
097300150526     C                   EXSR      SR_SPLIT
097400150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
097500150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
097600141028     C                   EVAL      VATTRC = 'E'
097700141028     C                   EXSR      WRIVAT
097800150526     C                   ENDIF
097900150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
098000150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
098100150526     C                   EVAL      VATTRC = 'E'
098200150526     C                   EXSR      WRIVAT
098300150526     C                   ENDIF
098400150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
098500150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
098600150526     C                   EVAL      VATTRC = 'E'
098700150526     C                   EXSR      WRIVAT
098800150526     C                   ENDIF
098900150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
099000150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
099100150526     C                   EVAL      VATTRC = 'E'
099200150526     C                   EXSR      WRIVAT
099300150526     C                   ENDIF
099400150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
099500150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
099600150526     C                   EVAL      VATTRC = 'E'
099700150526     C                   EXSR      WRIVAT
099800150526     C                   ENDIF
099900141028     C                   ENDIF
100000141028     C*
100100141028     C***
100200141028     C* ...tipo record '???' (PDF per packing-list)
100300141028     C                   EVAL      wTag = '???PDF'
100400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100500141028     C*
100600141028     C* ......VATNOT_P
100700141028     C                   if        *in53
100800141028     C*
100900141028     C* Solo se valorizzato RMN
101000141028     C                   if        VABRMA <> *blanks
101100141028     C                   eval      pIn_MaskPDF =
101200141028     C                                     %trim(%subst(wvindta:1292:100))
101300141028     C                   call      'TIS7P2SER'
101400141028     C                   parm      'W'           pIn_Opz           1
101500141028     C                   parm                    tivlrds
101600141028     C                   parm                    EDIVABDS
101700141028     C                   parm      '10'          pIn_CdIdAz        2
101800141028     C                   parm                    pIn_MaskPDF      50
101900141028     C                   parm      *blanks       pOut_Esito        1
102000141028     C                   endif
102100141028     C                   endif
102200141028     C*
102300141028     C                   ENDIF
102400141028     C*
102500141028     C***
102600141028     C* ...tipo record 'UNT' (fine bolla)
102700141028     C                   EVAL      wTag = 'UNT'
102800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102900150526     C                   IF        *IN90
103000141028     C  N31              ADD       1             §CTROKVB
103100141028     C  N31              exsr      CHKWRI
103200140117     C  N31              WRITE     EDIVAB00
103300150526     C                   ENDIF
103400150526     C                   ENDIF
103500010202     C*
103600000801     C* Ebbene...
103700000801     C                   ADD       1             §CTRMO
103800141028     C                   IF        wRecErr <> *zeros
103900000801     C                   ADD       1             §CTRNO
104000000801     C                   EVAL      vinflg = '2'
104100000801     C                   ENDIF
104200000801     C*
104300000801     C                   ENDSR
104400010202     C*----------------------------------------------------*
104500140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
104600010202     C*----------------------------------------------------*
104700020305     C     PREVAT        BEGSR
104800010202     C*
104900140117     C* Compongo il nome del membro da dare al EDIVATWR
105000010202     C                   eval      parmbr = vlrhdl
105100010202     C                   movel     'M'           parmbr
105200060113     C                   eval      parccm = vlrksc
105300010202     C                   eval      paropz = '1'
105400010202     C* Effettuo la chiamata al CLLE preposto
105500140117     C                   call(e)   'TITVEVTC'
105600010202     C                   parm                    parccm
105700010202     C                   parm                    parmbr
105800010202     C                   parm                    paropz
105900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
106000010202     C                   if        %error
106100010202     C                   movel     '1'           chkcall
106200010202     C                   else
106300010202     C                   movel     '0'           chkcall
106400010202     C                   endif
106500010202     C*
106600010202     C                   ENDSR
106700141028     C*----------------------------------------------------*
106800141028     C*  CONTROLLO NUMERICITA' CAMPI
106900141028     C*----------------------------------------------------*
107000141028     C     CHKNUM        BEGSR
107100141028     C*
107200141028     C                   IF        PiDecChr = *blanks
107300141028     C                   EVAL      PiDecChr = CharNUM
107400141028     C                   ENDIF
107500141028     C*
107600141028     C                   callp     UBISNUM_Check(PiStr
107700141028     C                                          :PiDecChr
107800141028     C                                          :PiVal
107900141028     C                                          :PiNum
108000141028     C                                          :PiInt)
108100141028     C*
108200141028     C                   ENDSR
108300141028     C***
108400141028
108500141028
108600141028     C*----------------------------------------------------*
108700141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
108800141028     C*----------------------------------------------------*
108900141028     C     SR_SPLIT      BEGSR
109000141028     C*
109100141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
109200141028     C                   CALL      'XSPLIT2'
109300141028     C                   PARM                    InStringa
109400141028     C                   PARM                    InSepara
109500141028     C                   PARM      '5'           InTypeOut
109600141028     C                   PARM      ''            wSkParam
109700141028     C                   PARM                    OutErrore
109800141028     C                   PARM                    OutMsg
109900141028     C                   MOVEA     wSkParam      SkSplitCSV_5
110000141028     C*
110100141028     C                   ENDSR
110200141028
110300141028
110400141028     C*----------------------------------------------------*
110500141028     C*  SCRITTURA BUFFER EDIVAT
110600141028     C*----------------------------------------------------*
110700141028     C     WRIVAT        BEGSR
110800141028     C*
110900141028     C                   IF        VATNOT <> *blanks
111000141028     C                   EVAL      VATFGS = VABFGS
111100141028     C                   EVAL      VATCCM = VABCCM
111200141028     C                   EVAL      VATLNP = VABLNP
111300141028     C                   EVAL      VATAAS = VABAAS
111400141028     C                   EVAL      VATNRS = VABNRS
111500141028     C                   EVAL      VATNSP = VABNSP
111600141028     C                   EVAL      VATCMR = VABCMR
111700141028     C                   EVAL      VATCNT = VABCNT
111800141028     C                   ADD       1             §CTROKVT
111900141028     C                   WRITE     EDIVAT00
112000141028     C                   ENDIF
112100141028     C*
112200141028     C                   ENDSR
112300000801
112400011113
112500011113     C*----------------------------------------------------*
112600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
112700011113     C*----------------------------------------------------*
112800011113     C     CHKIMPDIV     BEGSR
112900011113     C*
113000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
113100011113     C                   Z-ADD     *zeros        wrkDec            9 9
113200011113     C*
113300011113     C* Come prima cosa effettuo considerazioni sulla divisa
113400011113     C                   IF        vabIAS > *zeros
113500011113     C                   IF        vabVAS <> 'EUR'
113600011113     C                   EVAL      vabVAS =  'ITL'
113700011113     C                   ENDIF
113800011113     C                   ENDIF
113900011113     C*
114000011113     C                   IF        vabCAS > *zeros
114100011113     C                   IF        vabVCA <> 'EUR'
114200011113     C                   EVAL      vabVCA =  'ITL'
114300011113     C                   ENDIF
114400011113     C                   ENDIF
114500011113     C*
114600011113     C                   IF        vabVMD > *zeros
114700020305     C                   IF        vabVAD <> 'EUR'
114800011113     C                   EVAL      vabVAD =  'ITL'
114900011113     C                   ENDIF
115000011113     C                   ENDIF
115100011113     C*
115200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
115300011113     C                   Z-ADD     vabIAS        wrkDec
115400011113     C                   IF        wrkDec > *zeros
115500011113     C                   IF        vabVAS = 'ITL'
115600011113     C                   EVAL      vabIAS = *zeros
115700011113     C                   ENDIF
115800011113     C                   ENDIF
115900011113     C*
116000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
116100011113     C                   Z-ADD     vabCAS        wrkDec
116200011113     C                   IF        wrkDec > *zeros
116300011113     C                   IF        vabVCA = 'ITL'
116400011113     C                   EVAL      vabCAS = *zeros
116500011113     C                   ENDIF
116600011113     C                   ENDIF
116700011113     C*
116800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
116900011113     C                   Z-ADD     vabVMD        wrkDec
117000011113     C                   IF        wrkDec > *zeros
117100011113     C                   IF        vabVAD = 'ITL'
117200011113     C                   EVAL      vabVMD = *zeros
117300011113     C                   ENDIF
117400011113     C                   ENDIF
117500011113     C*
117600011113     C                   ENDSR
117700011113     C***
117800011113
117900011113
118000000801
118100000801
118200990920      /TITLE Invio dei dati al punto operativo.
118300010202     C     invio         BEGSR
118400990920     C*
118500140117     C* 1° invio EDIVAT
118600010201     C                   reset                   dscmz
118700010201     C                   move      vlrpoi        cmzdst
118800140117     C                   eval      cmzfld = 'EDIVATWR'
118900010201     C                   eval      cmzmbd = vlrhdl
119000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
119100021009     C***                if        prmfir = *blanks
119200140117     C                   eval      cmzfla = 'EDIVAT0F'
119300140117     C                   eval      cmzmba = 'EDIVAT0F'
119400021009     C***                else
119500021009     C***                eval      cmzfla = prmfir
119600021009     C***                eval      cmzmba = prmfir
119700021009     C***                endif
119800010201     C                   eval      cmznrr = *zeros
119900020305     C                   move      §ctrokvt      cmznrr
120000021018     C                   eval      cmzlba = vlrfl1
120100010201     C                   call(e)   'TIS711C'
120200010201     C                   parm                    dscmz
120300010201     C                   parm      *blanks       esito
120400010205     C                   if        %error
120500010205     C                             or cmzerr = '1'
120600010205     C                             or esito  = '1'
120700010205     C                   eval      wrkesito = '3'
120800010205     C                   else
120900010201     C*
121000140117     C* 2° invio EDIVAB
121100010201     C                   reset                   dscmz
121200010201     C                   move      vlrpoi        cmzdst
121300010201     C                   eval      cmzfld = vlrfou
121400010201     C                   eval      cmzmbd = vlrhdl
121500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
121600021009     C***                if        prmfir = *blanks
121700140117     C                   eval      cmzfla = 'EDIVAB0F'
121800140117     C                   eval      cmzmba = 'EDIVAB0F'
121900021009     C***                else
122000021009     C***                eval      cmzfla = prmfir
122100021009     C***                eval      cmzmba = prmfir
122200021009     C***                endif
122300010201     C                   eval      cmznrr = *zeros
122400010201     C                   move      §ctrokvb      cmznrr
122500021018     C                   eval      cmzlba = vlrfl1
122600010201     C                   call(e)   'TIS711C'
122700010201     C                   parm                    dscmz
122800010201     C                   parm      *blanks       esito
122900010201     C                   if        %error
123000010201     C                             or cmzerr = '1'
123100010201     C                             or esito  = '1'
123200010201     C                   eval      wrkesito = '3'
123300010201     C                   endif
123400010205     C                   endif
123500990920     C*
123600000613     C                   ENDSR
123700000613     C***
123800070411
123900141028
124000070411     C     *pssr         BEGSR
124100070411     C*
124200070411     C                   if        %open(tivin00r)
124300070411     C                   close     tivin00r
124400070411     C                   endif
124500140117     C                   if        %open(edivabwr)
124600140117     C                   close     edivabwr
124700070411     C                   endif
124800140117     C                   if        %open(edivatwr)
124900140117     C                   close     edivatwr
125000070411     C                   endif
125100070411     C*
125200070411     C* Effettuo la chiamata al CLLE preposto
125300140117     C                   call(e)   'TITVEVTC'
125400070411     C                   parm                    parccm
125500070411     C                   parm                    parmbr
125600070411     C                   parm      '2'           paropz
125700070411     C*
125800070411     C                   eval      wrkesito = '2'
125900070411     C*
126000070411     C                   seton                                        LR
126100070411     C*
126200070411     C                   ENDSR     '*CANCL'
126300070411     C***
126400170221
126500170221
126600170221     C*--------------------------------------------------------
126700170221     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
126800170221     C*--------------------------------------------------------
126900170221     C     CARTAB        BEGSR
127000170221     C*
127100170221     C* TABELLA '15' - NAZIONI
127200170221     C                   clear                   skNAZISO
127300170221     C                   clear                   skNAZBAR
127400170221     C                   eval      tblKUT = 1
127500170221     C                   eval      tblCOD = '15'
127600170221     C     KEYtabP       setll     tabel00f
127700170221     C     KEYtabP       reade     tabel00f
127800170221     C                   dow       not %eof(tabel00f)
127900170221     C                   if        tblFLG = *blanks
128000170221     C                   movel(p)  tblUNI        ds15
128100170221     C                   add       1             jNAZ
128200170221     C                   eval      skNAZBAR(jNAZ) = tblKEY
128300170221     C                   eval      skNAZISO(jNAZ) = §15COD
128400170221     C                   endif
128500170221     C     KEYtabP       reade     tabel00f
128600170221     C                   enddo
128700170221     C*
128800170221     C                   ENDSR
128900170221     C***
129000070411
129100990910
129200000613     C     *inzsr        BEGSR
129300990910     C*
129400990910     C     *entry        plist
129500990920     C                   parm                    tivlrds
129600990921     C                   parm      wrkesito      esito
129700000724     C                   parm                    prmlit
129800000710     C                   parm                    prmfir
129900000613     C*
130000000830     C* CALCOLA LA DATA CORRENTE
130100140117     C                   time                    wn14             14 0
130200140117     C                   movel     wn14          oracor            6 0          *ORA
130300100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
130400100722     C                   eval      datcor = %dec(%date() : *ISO)
130500170221     C*
130600170221     C* Chiave su TABEL00F - parziale
130700170221     C     KEYtabP       KLIST
130800170221     C                   KFLD                    tblKUT
130900170221     C                   KFLD                    tblCOD
131000000830     C*
131100000613     C                   ENDSR
131200000613     C***
