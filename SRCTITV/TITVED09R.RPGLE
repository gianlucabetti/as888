000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140117     FEDIVABwr  O    E             DISK    usropn
000600140117     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600140117     D edivabds      e ds                  extname(edivab0f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600141028     D wTag            s            128    VARYING INZ
002700141028     D wRecErr         s              1  0 INZ(*zeros)
002800141028     D wFlgCAS         s              1    INZ(*blanks)
002900141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003000150527     D wNOTE           s             70    INZ(*blanks)
003100141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003200141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003800141028
003900141028     D*------------------
004000141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004100141028     D*------------------
004200141028     D CharNUM         S              1
004300141028     D CharSOS         S              1
004400141028     D posDaDft        S              3  0 INZ(*zeros)
004500141028     D posADft         S              3  0 INZ(*zeros)
004600141028     D wGiroDft        s              1  0 INZ(*zeros)
004700141028
004800141028
004900141028     D*------------------
005000141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005100141028     D*------------------
005200141028     D InStringa       S          65535A   VARYING                              (stringa input)
005300141028     D InSepara        S             10A                                        (separatore)
005400141028     D InTypeOut       S              1A                                        (tipo output)
005500141028     D wSkParam        S          65535A                                        (output)
005600141028     D OutErrore       S              1A                                        (flag errore)
005700141028     D OutMsg          S             80A                                        (messaggio errore)
005800141028     D SkSplitCSV_5    S            256    DIM(256)
005900141028
006000000830
006100041025     D*------------------
006200041025     D* DS REPERIMENTO NUMERATORE
006300041025     D*------------------
006400041025     D trul33ds      e ds                  inz
006500041025     D*------------------
006600041025     D* DS ARCHITETTURA
006700041025     D*------------------
006800041025     D kpjba         e ds                  inz
006900041025     D*------------------
007000990908
007100141028
007200141028     D*------------------
007300141028     D* LINKING A DEFINIZIONI ESTERNE
007400141028     D*------------------
007500141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700150609
007800141028
007900150609     D*-------------------
008000150609     D* COSTANTI
008100150609     D*-------------------
008200150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400141028
008500010201
008600010201
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000150526     C                   setoff                                       90
009100000613     C*
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300070719     C*
009400070719     C* Esegue lancio TISI95R solo x chiusura
009500070719     C                   CLEAR                   TISI95DS
009600070719     C                   EVAL      I95TLA = 'C'
009700070719     C                   CALL      'TISI95R'
009800070719     C                   PARM                    TISI95DS
009900000613     C*
010000010202     C* Effettuo la chiamata al CLLE preposto
010100140922     C                   call(e)   'TITVEVTC'
010200140922     C                   parm                    parccm
010300140922     C                   parm                    parmbr
010400140922     C                   parm      '2'           paropz
010500170731     C*
010600170731     C                   if        *in53
010700170731     C                   call      'TIS7P2SER'
010800170731     C                   parm      'C'           pIn_Opz           1
010900170731     C                   parm                    tivlrds
011000170731     C                   parm                    EDIVABDS
011100170731     C                   parm      *blanks       pIn_CdIdAz        2
011200170731     C                   parm      *blanks       pIn_MaskPDF      50
011300170731     C                   parm      *blanks       pOut_Esito        1
011400170731     C                   endif
011500000616     C*
011600010201     C                   seton                                        LR
011700990908
011800000801
011900910830     C*--------------------------------------------------------
012000140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012100910830     C*--------------------------------------------------------
012200040526     C     RWFILE        BEGSR
012300990910     C*
012400990914     C                   if        not %open(tivin00r)
012500990908     C                   open      tivin00r
012600990914     C                   endif
012700140117     C                   if        not %open(edivabwr)
012800140117     C                   open      edivabwr
012900990914     C                   endif
013000141028     C*
013100140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
013200020305     C                   exsr      prevat
013300010201     C*
013400010202     C                   if        chkcall = '0'
013500010202     C*
013600140117     C                   if        not %open(edivatwr)
013700140117     C                   open      edivatwr
013800010201     C                   endif
013900990910     C*
014000010201     C                   clear                   §CTROKVB          5 0
014100020305     C                   clear                   §CTROKVT          5 0
014200000801     C                   clear                   §CTRMO            5 0
014300000801     C                   clear                   §CTRNO            5 0
014400141028     C*
014500141028     C* Inizializzazioni fuori ciclo
014600141028     C                   EXSR      INZVAR
014700140117     C*
014800140902     C                   if        *in53
014900140117     C                   call      'TIS7P2SER'
015000140117     C                   parm      'O'           pIn_Opz           1
015100140117     C                   parm                    tivlrds
015200140117     C                   parm                    EDIVABDS
015300140117     C                   parm      *blanks       pIn_CdIdAz        2
015400140117     C                   parm      *blanks       pIn_MaskPDF      50
015500140117     C                   parm      *blanks       pOut_Esito        1
015600140902     C                   endif
015700130708     C*
015800921023     C                   DO        *HIVAL
015900990913     C*
016000990915     C                   READ      tivin00r                               70
016100040910     C                   if        vindta > *blanks
016200000613     C                   add       1             rrnum
016300000801     C*
016400000801     C                   if        *in70 = *off
016500000801     C                             and
016600000801     C                             (vinflg = *blanks
016700000801     C                              or vinflg = '0'
016800000801     C                              or vinflg = '2')
016900000801     C*
017000000801     C                   clear                   vinmsg
017100141028     C*
017200141028     C* "normalizzo" il dato di input
017300141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
017400141028     C                                             %len(%trim(vindta))-1))
017500160209     C*
017600160209     C* Elimino le "duple di controllo EDIFACT"
017700160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017800160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
017900160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018000160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018100160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
018200040910     C*
018300040910     C* Eseguo routine d traduzione
018400040910     C                   exsr      impvabvat
018500040802     C*
018600010305     C                   endif
018700000905     C*
018800000905     C                   else
018900000905     C                   eval      vinflg = '1'
019000000905     C                   endif
019100000905     C*
019200000905     C  N70              update    tivin000
019300000905     C*
019400991022     C  N70              ENDdo
019500100722     C*
019600100722     C* Scarico i buffer testata ancora "in canna"
019700140922     C*
019800141028     C***  N31              ADD       1             §CTROKVB
019900141028     C***  N31              exsr      CHKWRI
020000141028     C***  N31              WRITE     EDIVAB00
020100010202     C*
020200010202     C                   endif
020300990910
020400990910     C* Se non ci sono record con errori ...
020500000710     C                   if        §ctrno = 0
020600990910     C* ... restituisco esito OK.
020700990921     C                   eval      wrkesito = '0'
020800990910     C                   else
020900010201     C                   if        §ctrokvb > 0
021000990921     C                   eval      wrkesito = '1'
021100000710     C                   else
021200000710     C                   eval      wrkesito = '2'
021300990910     C                   endif
021400000710     C                   endif
021500990910     C*
021600990914     C                   if        %open(tivin00r)
021700990908     C                   close     tivin00r
021800990914     C                   endif
021900140117     C                   if        %open(edivabwr)
022000140117     C                   close     edivabwr
022100990914     C                   endif
022200140117     C                   if        %open(edivatwr)
022300140117     C                   close     edivatwr
022400010201     C                   endif
022500990910     C*
022600010201     C                   if        §ctrokvb > 0
022700000724     C                             and vlrpoi <> *zeros
022800010202     C                   exsr      invio
022900990920     C                   endif
023000990920     C*
023100910830     C                   ENDSR
023200000613     C***
023300140922
023400140922     C*----------------------------------------------------*
023500140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
023600140922     C*----------------------------------------------------*
023700140922     C     CHKWRI        BEGSR
023800140922     C*
023900141028     C                   if        VABRMN = *zeros
024000141028     C                   eval      VABRMN = VABNSP
024100141028     C                   endif
024200141028     C*
024300141028     C                   if        VABPRD = *blanks
024400141028     C* ......VABPRD
024500141028     C* Reperisco la provincia dal CAP e dalla località
024600141028     C                   IF        VABLOD <> *blanks AND
024700141028     C                             VABCAD <> *blanks AND
024800141028     C                             VABNZD  = *blanks
024900141028     C                   CLEAR                   TISI95DS
025000141028     C                   EVAL      I95TCN = '3'
025100141028     C                   Z-ADD     datcor        I95DAT
025200141028     C                   EVAL      I95CAP = VABCAD
025300141028     C                   EVAL      I95LOC = VABLOD
025400141028     C                   CALL      'TISI95R'
025500141028     C                   PARM                    TISI95DS
025600141028     C                   EVAL      VABPRD = O95PRV
025700141028     C                   ENDIF
025800141028     C                   endif
025900140922     C*
026000140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
026100141028     C                   IF        wFlgCAS <> '0'
026200140922     C                   IF        VABCBO = '1'
026300140922     C                   EVAL      VABCBO = '4'
026400140922     C                   ELSE
026500140922     C                   EVAL      VABCBO = '6'
026600140922     C                   ENDIF
026700140922     C                   ENDIF
026800140922     C*
026900140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027000141028     C                   EXSR      CHKIMPDIV
027100150527     C*
027200150527     C* Scompongo le note
027300150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
027400150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
027500140922     C*
027600140922     C                   ENDSR
027700141028     C*----------------------------------------------------*
027800141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900141028     C*----------------------------------------------------*
028000141028     C     INZVAR        BEGSR
028100141028     C*
028200141028     C* Inizializzo variabili di wrk
028300141028     C                   Z-ADD     *zeros        Num5_0            5 0
028400141028     C                   MOVEL     '0'           wFlgCAS
028500141028     C                   MOVEL     *blanks       wVATNOT_A
028600141028     C                   MOVEL     *blanks       wVATNOT_B
028700141028     C                   MOVEL     *blanks       wVATNOT_E
028800141028     C                   MOVEL     *blanks       wVATNOT_I
028900141028     C                   MOVEL     *blanks       wVATNOT_J
029000141028     C                   MOVEL     *blanks       wVATNOT_S
029100141028     C                   MOVEL     *blanks       wVATNOT_P
029200141028     C                   CLEAR                   SkSplitCSV_5
029300150527     C                   MOVEL     *blanks       wNOTE
029400141028     C*
029500141028     C* Reimposto i valori di default
029600141028     C                   EXSR      DEFCAM
029700141028     C*
029800141028     C                   ENDSR
029900141028     C*----------------------------------------------------*
030000141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
030100141028     C*----------------------------------------------------*
030200141028     C     DEFCAM        BEGSR
030300141028     C*
030400141028     C                   CLEAR                   EDIVAB00
030500141028     C                   CLEAR                   EDIVAT00
030600141028     C*
030700141028     C* Imposto i valori bolla di default
030800141028     C                   EVAL      VABCTR = 000
030900141028     C                   EVAL      VABCBO = '1'
031000141104     C                   EVAL      VABTSP = 'C'
031100141028     C*
031200141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
031300141028     C* e delimitatore testo.
031400141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
031500141028     C*
031600141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031700141028     C                   EVAL      CharSOS = CharNUM
031800141028     C*
031900141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
032000141028     C                   SETOFF                                       53
032100141028     C                   SELECT
032200141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
032300141028     C                   SETON                                        53
032400141028     C                   ENDSL
032500141028     C*
032600141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032700141028     C                   EVAL      posDaDft = 1
032800141028     C                   EVAL      posADft  = 0
032900141028     C                   EVAL      wGiroDft = 0
033000141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
033100141028     C                             posDaDft > 0
033200141028     C*
033300141028     C* Gestisco il 1° giro
033400141028     C                   IF        wGiroDft = 0
033500141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
033600141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033700141028     C* Incremento il contatore dei "giri"
033800141028     C                   EVAL      wGiroDft = 1
033900141028     C                   ELSE
034000141028     C                   EVAL      posDaDft = posADft
034100141028     C                   ENDIF
034200141028     C* Eseguo lo scan x trovare la fine del campo corrente
034300141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
034400141028     C*
034500141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
034600141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
034700141028     C                   IF        posDaDft > 0 AND
034800141028     C                             posADft  > 0
034900141028     C* NCL
035000141028     C                   IF        %subst(
035100141028     C                             %subst(vlrppt:posDaDft+1:
035200141028     C                             posADft-posDaDft-1):1:3)
035300141028     C                             = 'NCL'
035400141028     C                   EVAL      PiStr=%trim(%subst(
035500141028     C                             %subst(vlrppt:posDaDft+1:
035600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035700141028     C                   EXSR      CHKNUM
035800141028     C                   IF        PiInt=*on
035900141028     C                   Z-ADD     PiVal         VABNCL
036000141028     C                   ENDIF
036100141028     C                   ENDIF
036200141028     C* CCM
036300141028     C                   IF        %subst(
036400141028     C                             %subst(vlrppt:posDaDft+1:
036500141028     C                             posADft-posDaDft-1):1:3)
036600141028     C                             = 'CCM'
036700141028     C                   EVAL      PiStr=%trim(%subst(
036800141028     C                             %subst(vlrppt:posDaDft+1:
036900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037000141028     C                   EXSR      CHKNUM
037100141028     C                   IF        PiInt=*on
037200141028     C                   Z-ADD     PiVal         VABCCM
037300141028     C                   ENDIF
037400141028     C                   ENDIF
037500141028     C* LNP
037600141028     C                   IF        %subst(
037700141028     C                             %subst(vlrppt:posDaDft+1:
037800141028     C                             posADft-posDaDft-1):1:3)
037900141028     C                             = 'LNP'
038000141028     C                   EVAL      PiStr=%trim(%subst(
038100141028     C                             %subst(vlrppt:posDaDft+1:
038200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038300141028     C                   EXSR      CHKNUM
038400141028     C                   IF        PiInt=*on
038500141028     C                   Z-ADD     PiVal         VABLNP
038600141028     C                   ENDIF
038700141028     C                   ENDIF
038800141028     C* NRS
038900141028     C                   IF        %subst(
039000141028     C                             %subst(vlrppt:posDaDft+1:
039100141028     C                             posADft-posDaDft-1):1:3)
039200141028     C                             = 'NRS'
039300141028     C                   EVAL      PiStr=%trim(%subst(
039400141028     C                             %subst(vlrppt:posDaDft+1:
039500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039600141028     C                   EXSR      CHKNUM
039700141028     C                   IF        PiInt=*on
039800141028     C                   Z-ADD     PiVal         VABNRS
039900141028     C                   ENDIF
040000141028     C                   ENDIF
040100141028     C* CTR
040200141028     C                   IF        %subst(
040300141028     C                             %subst(vlrppt:posDaDft+1:
040400141028     C                             posADft-posDaDft-1):1:3)
040500141028     C                             = 'CTR'
040600141028     C                   EVAL      PiStr=%trim(%subst(
040700141028     C                             %subst(vlrppt:posDaDft+1:
040800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900141028     C                   EXSR      CHKNUM
041000141028     C                   IF        PiInt=*on
041100141028     C                   Z-ADD     PiVal         VABCTR
041200141028     C                   ENDIF
041300141028     C                   ENDIF
041400141028     C* PKB
041500141028     C                   IF        %subst(
041600141028     C                             %subst(vlrppt:posDaDft+1:
041700141028     C                             posADft-posDaDft-1):1:3)
041800141028     C                             = 'PKB'
041900141028     C                   EVAL      PiStr=%trim(%subst(
042000141028     C                             %subst(vlrppt:posDaDft+1:
042100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200141028     C                   EXSR      CHKNUM
042300141028     C                   IF        PiNum=*on
042400141028     C                   Z-ADD     PiVal         VABPKB
042500141028     C                   ENDIF
042600141028     C                   ENDIF
042700141028     C* VLB
042800141028     C                   IF        %subst(
042900141028     C                             %subst(vlrppt:posDaDft+1:
043000141028     C                             posADft-posDaDft-1):1:3)
043100141028     C                             = 'VLB'
043200141028     C                   EVAL      PiStr=%trim(%subst(
043300141028     C                             %subst(vlrppt:posDaDft+1:
043400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500141028     C                   EXSR      CHKNUM
043600141028     C                   IF        PiNum=*on
043700141028     C                   Z-ADD     PiVal         VABVLB
043800141028     C                   ENDIF
043900141028     C                   ENDIF
044000141028     C* QFT
044100141028     C                   IF        %subst(
044200141028     C                             %subst(vlrppt:posDaDft+1:
044300141028     C                             posADft-posDaDft-1):1:3)
044400141028     C                             = 'QFT'
044500141028     C                   EVAL      PiStr=%trim(%subst(
044600141028     C                             %subst(vlrppt:posDaDft+1:
044700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800141028     C                   EXSR      CHKNUM
044900141028     C                   IF        PiNum=*on
045000141028     C                   Z-ADD     PiVal         VABQFT
045100141028     C                   ENDIF
045200141028     C                   ENDIF
045300141028     C* CBO
045400141028     C                   IF        %subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):1:3)
045700141028     C                             = 'CBO'
045800141028     C                   EVAL      VABCBO=%trim(%subst(
045900141028     C                             %subst(vlrppt:posDaDft+1:
046000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100141028     C                   ENDIF
046200141028     C* TSP
046300141028     C                   IF        %subst(
046400141028     C                             %subst(vlrppt:posDaDft+1:
046500141028     C                             posADft-posDaDft-1):1:3)
046600141028     C                             = 'TSP'
046700141028     C                   EVAL      VABTSP=%trim(%subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000141028     C                   ENDIF
047100141028     C* VAS
047200141028     C                   IF        %subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):1:3)
047500141028     C                             = 'VAS'
047600141028     C                   EVAL      VABVAS=%trim(%subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900141028     C                   ENDIF
048000141028     C* VCA
048100141028     C                   IF        %subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):1:3)
048400141028     C                             = 'VCA'
048500141028     C                   EVAL      VABVCA=%trim(%subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800141028     C                   ENDIF
048900141028     C* TIC
049000141028     C                   IF        %subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):1:3)
049300141028     C                             = 'TIC'
049400141028     C                   EVAL      VABTIC=%trim(%subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700141028     C                   ENDIF
049800141028     C* GCA
049900141028     C                   IF        %subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):1:3)
050200141028     C                             = 'GCA'
050300141028     C                   EVAL      VABGCA=%trim(%subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600141028     C                   ENDIF
050700141028     C* CTM
050800141028     C                   IF        %subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):1:3)
051100141028     C                             = 'CTM'
051200141028     C                   EVAL      VABCTM=%trim(%subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500141028     C                   ENDIF
051600141028     C* FFD
051700141028     C                   IF        %subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):1:3)
052000141028     C                             = 'FFD'
052100141028     C                   EVAL      VABFFD=%trim(%subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400141028     C                   ENDIF
052500141028     C* VAD
052600141028     C                   IF        %subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):1:3)
052900141028     C                             = 'VAD'
053000141028     C                   EVAL      VABVAD=%trim(%subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300141028     C                   ENDIF
053400141028     C* GMA
053500141028     C                   IF        %subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):1:3)
053800141028     C                             = 'GMA'
053900141028     C                   EVAL      VABGMA=%trim(%subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200141028     C                   ENDIF
054300141028     C* GGA
054400141028     C                   IF        %subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):1:3)
054700141028     C                             = 'GGA'
054800141028     C                   EVAL      VABGGA=%trim(%subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100141028     C                   ENDIF
055200141028     C* GVA
055300141028     C                   IF        %subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):1:3)
055600141028     C                             = 'GVA'
055700141028     C                   EVAL      VABGVA=%trim(%subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000141028     C                   ENDIF
056100141028     C* TC1
056200141028     C                   IF        %subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):1:3)
056500141028     C                             = 'TC1'
056600141028     C                   EVAL      VABTC1=%trim(%subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900141028     C                   ENDIF
057000141028     C* TC2
057100141028     C                   IF        %subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):1:3)
057400141028     C                             = 'TC2'
057500141028     C                   EVAL      VABTC2=%trim(%subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800141028     C                   ENDIF
057900141028     C* VATTRC
058000141028     C                   IF        %subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):1:3)
058300141028     C                             = 'TRC'
058400141028     C                   EVAL      VATTRC=%trim(%subst(
058500141028     C                             %subst(vlrppt:posDaDft+1:
058600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700141028     C                   ENDIF
058800141028     C* ...
058900141028     C                   ENDIF
059000141028     C                   ENDDO
059100141028     C*
059200141028     C                   ENDSR
059300000801     C*----------------------------------------------------*
059400140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
059500000801     C*----------------------------------------------------*
059600040910     C     IMPVABVAT     BEGSR
059700040910     C*
059800040910     C* Traduzione relativa ai tipi record del file del cliente
059900040910     C*
060000071210     C*
060100071210     C***
060200141028     C* ...tipo record 'UNB' (info CMR)
060300141028     C                   EVAL      wTag = 'UNB'
060400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
060500141028     C                   MOVEL     *blanks       savCMR           35
060600141028     C                   EVAL      InStringa =
060700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
060800141028     C                   EVAL      InSepara = '+'
060900141028     C                   EXSR      SR_SPLIT
061000141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
061100140922     C                                      %char(datcor) + ' ' + %char(oracor)
061200130620     C                   ENDIF
061300141028     C*
061400141028     C***
061500150526     C* ...tipo record 'CNI' (rottuta di codice spedizione)
061600150526     C                   EVAL      wTag = 'CNI'
061700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061800150526     C*
061900150526     C* Scarico buffer spedizione elaborata
062000150526     C                   IF        *IN90
062100150526     C  N31              ADD       1             §CTROKVB
062200150526     C  N31              exsr      CHKWRI
062300150526     C  N31              WRITE     EDIVAB00
062400150526     C                   ENDIF
062500150526     C                   SETON                                        90
062600150526     C*
062700150526     C* Inizializzazioni per nuova spedizione
062800141028     C                   SETOFF                                       31
062900141028     C                   eval      vinflg = '1'
063000141028     C* ......inizializzazioni iniziali e formati record file Bartolini
063100141028     C                   EXSR      INZVAR
063200141028     C* ......valorizzazione campi fissi
063300141028     C                   MOVEL     datcor        VABAAS
063400141028     C                   MOVE      datcor        VABMGS
063500141028     C                   MOVE(P)   vlrpoi        VABFGS
063600141028     C                   EVAL      VABCMR = savCMR
063700141028     C                   EVAL      VABDCM = datcor
063800141028     C                   EVAL      VABDTS = datcor
063900141028     C                   EVAL      VABCNT = 1
064000141028     C* ......NSP => Stacco un numeratore da AZNUM
064100141028     C                   clear                   TRUL33DS
064200141028     C                   eval      I33OPE = *zeros
064300141028     C                   eval      I33CNU = 302
064400141028     C                   eval      I33NUM = 1
064500141028     C                   movel     TRUL33DS      KPJBU
064600141028     C                   call      'TRUL33R'
064700141028     C                   parm                    KPJBA
064800141028     C                   movel     KPJBU         TRUL33DS
064900141028     C                   if        O33ERR = *zeros
065000141028     C                   z-add     O33NRF        VABNSP
065100141028     C                   else
065200141028     C                   Z-ADD     1             wRecErr
065300141028     C                   SETON                                        31
065400141028     C                   EVAL      vinmsg = %trimr(vinmsg)
065500141028     C                             + ' ' + 'VABNSP VATNSP'
065600141028     C                   endif
065700150811     C*
065800150811     C***
065900150811     C* ...tipo record 'CNI' (riferimento spedizione)
066000150811     C                   EVAL      InStringa =
066100150811     C                                %subst(wvindta:%len(%trim(wTag))+2)
066200150811     C                   EVAL      InSepara = '+'
066300150811     C                   EXSR      SR_SPLIT
066400150811     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
066500150811     C                   EXSR      CHKNUM
066600150811     C                   IF        PiNum=*on
066700150811     C                   Z-ADD     PiVal         VABRMN
066800150811     C                   ELSE
066900150811     C                   Z-ADD     1             VABRMN
067000150811     C                   ENDIF
067100150811     C*
067200141028     C                   ENDIF
067300141028     C*
067400141028     C***
067500150526     C* ...tipo record 'RFF+FF' (riferimento spedizione)
067600150526     C                   EVAL      wTag = 'RFF+FF'
067700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067800150526     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
067900150526     C                   EXSR      CHKNUM
068000150526     C                   IF        PiInt=*on
068100150526     C                   Z-ADD     PiVal         VABRMN
068200150526     C                   ELSE
068300150526     C                   EVAL      VABRMN = 1
068400150526     C                   Z-ADD     1             wRecErr
068500150526     C                   ENDIF
068600141028     C                   ENDIF
068700150526     C*
068800150526     C***
068900150526     C* ...tipo record 'RFF+CR' (riferimento spedizione)
069000150526     C                   EVAL      wTag = 'RFF+CR'
069100150526     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069200150526     C                   EVAL      VABRMA =
069300150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
069400150526     C                   ENDIF
069500150810     C*
069600150810     C***
069700150810     C* ...tipo record 'RFF+AFC' (riferimento spedizione)
069800150810     C                   EVAL      wTag = 'RFF+AFC'
069900150810     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070000150810     C                   EVAL      VABRMA =
070100150810     C                                %subst(wvindta:%len(%trim(wTag))+2)
070200150810     C                   ENDIF
070300150527     C*
070400150527     C***
070500150527     C* ...tipo record 'DTM+2' (data consegna richiesta)
070600150527     C                   EVAL      wTag = 'DTM+2'
070700150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070800150527     C                   EVAL      PiStr=%subst(
070900150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
071000150527     C                   EXSR      CHKNUM
071100150527     C                   IF        PiInt=*on
071200150527     C                   Z-ADD     PiVal         VABDCR
071300150527     C                   ELSE
071400150527     C                   Z-ADD     1             wRecErr
071500150527     C                   ENDIF
071600150527     C                   ENDIF
071700141028     C*
071800141028     C***
071900141028     C* ...tipo record 'NAD+CN' (destinatario)
072000141028     C                   EVAL      wTag = 'NAD+CN'
072100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072200141028     C                   EVAL      InStringa =
072300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
072400141028     C                   EVAL      InSepara = '+'
072500141028     C                   EXSR      SR_SPLIT
072600141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
072700141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
072800141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
072900141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
073000141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
073100141028     C                   IF        VABNZD = 'I'   OR
073200141028     C                             VABNZD = 'IT'  OR
073300141028     C                             VABNZD = 'ITA'
073400141028     C                   EVAL      VABNZD = *blanks
073500141028     C                   ENDIF
073600141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
073700141028     C                   EVAL      InSepara = ':'
073800141028     C                   EXSR      SR_SPLIT
073900141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
074000141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
074100141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
074200141028     C                                      %trim(SkSplitCSV_5(4))+
074300141028     C                                      %trim(SkSplitCSV_5(5))
074400141028     C                   ENDIF
074500150609     C*
074600150609     C***
074700150609     C* ...tipo record 'NAD+CZ' (mittente originale)
074800150609     C                   EVAL      wTag = 'NAD+CZ'
074900150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075000150609     C                   EVAL      InStringa =
075100150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
075200150609     C                   EVAL      InSepara = '+'
075300150609     C                   EXSR      SR_SPLIT
075400150807     C*
075500150807     C* Verifico eventuale forzatura codice mittente
075600150807     C                   IF        VABCCM <> 2200955
075700150807     C                   SELECT
075800160301     C                   WHEN      %subst(SkSplitCSV_5(1):1:7) = '0232008' OR
075900160301     C                             %subst(SkSplitCSV_5(1):1:7) = '0247700'
076000150807     C                   EVAL      VABCCM = 2200956
076100150807     C                   OTHER
076200150807     C                   EVAL      VABCCM = 2200957
076300150807     C                   ENDSL
076400150807     C                   ENDIF
076500150807     C*
076600150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
076700150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
076800150609     C                   IF        VABNMO = 'I'   OR
076900150609     C                             VABNMO = 'IT'  OR
077000150609     C                             VABNMO = 'ITA'
077100150609     C                   EVAL      VABNMO = *blanks
077200150609     C                   ENDIF
077300150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
077400150609     C                   EVAL      InSepara = ':'
077500150609     C                   EXSR      SR_SPLIT
077600150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
077700150609     C                   ENDIF
077800141104     C*
077900141104     C***
078000141104     C* ...tipo record 'NAD+UC' (destinatario)
078100141104     C                   EVAL      wTag = 'NAD+UC'
078200141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078300141104     C                   EVAL      InStringa =
078400141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
078500141104     C                   EVAL      InSepara = '+'
078600141104     C                   EXSR      SR_SPLIT
078700141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
078800141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
078900141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
079000141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
079100141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
079200141104     C                   IF        VABNZD = 'I'   OR
079300141104     C                             VABNZD = 'IT'  OR
079400141104     C                             VABNZD = 'ITA'
079500141104     C                   EVAL      VABNZD = *blanks
079600141104     C                   ENDIF
079700141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
079800141104     C                   EVAL      InSepara = ':'
079900141104     C                   EXSR      SR_SPLIT
080000141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
080100141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
080200141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
080300141104     C                                      %trim(SkSplitCSV_5(4))+
080400141104     C                                      %trim(SkSplitCSV_5(5))
080500141104     C                   ENDIF
080600141028     C*
080700141028     C***
080800141028     C* ...tipo record 'GID' (colli)
080900150609     C                   EVAL      wTag = 'GID+'
081000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081100141028     C                   EVAL      InStringa =
081200141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
081300141028     C                   EVAL      InSepara = '+'
081400141028     C                   EXSR      SR_SPLIT
081500150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
081600150526     C                   EVAL      InSepara = ':'
081700150526     C                   EXSR      SR_SPLIT
081800150526     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
081900141028     C                   EXSR      CHKNUM
082000141028     C                   IF        PiInt=*on
082100150526     C                   ADD       PiVal         VABNCL
082200141028     C                   ELSE
082300141028     C                   Z-ADD     1             wRecErr
082400141028     C                   ENDIF
082500141028     C                   ENDIF
082600150527     C*
082700150527     C***
082800150527     C* ...tipo record 'MOA+AAA' (contrassegno / divisa)
082900150527     C                   EVAL      wTag = 'MOA+AAA'
083000150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083100150527     C                   EVAL      wFlgCAS = '1'
083200150527     C                   EVAL      InStringa =
083300150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
083400150527     C                   EVAL      InSepara = ':'
083500150527     C                   EXSR      SR_SPLIT
083600150527     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
083700150527     C                   EXSR      CHKNUM
083800150527     C                   IF        PiNum=*on
083900150527     C                   ADD       PiVal         VABCAS
084000150527     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
084100150527     C                   ELSE
084200150527     C                   Z-ADD     1             wRecErr
084300150527     C                   ENDIF
084400150527     C                   ENDIF
084500141028     C*
084600141028     C***
084700141028     C* ...tipo record 'FTX+AAA' (natura merce)
084800141028     C                   EVAL      wTag = 'FTX+AAA'
084900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085000141028     C                   EVAL      InStringa =
085100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
085200141028     C                   EVAL      InSepara = '+'
085300141028     C                   EXSR      SR_SPLIT
085400141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
085500141028     C                   ENDIF
085600150622     C*
085700150622     C***
085800150622     C* ...tipo record 'TSR+' (tipo servizio)
085900150622     C                   EVAL      wTag = 'TSR+'
086000150622     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086100150622     C                   EVAL      InStringa =
086200150622     C                                %subst(wvindta:%len(%trim(wTag))+2)
086300150622     C                   EVAL      InSepara = '+'
086400150622     C                   EXSR      SR_SPLIT
086500150622     C                   SELECT
086600150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = '3'
086700150622     C                   EVAL      VABTSP = 'C'
086800150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'E'
086900150622     C                   EVAL      VABTSP = 'E'
087000150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'H'
087100150622     C                   EVAL      VABTSP = 'H'
087200150622     C                   ENDSL
087300150622     C                   ENDIF
087400150527     C*
087500150527     C***
087600150527     C* ...tipo record 'FTX+DIN' (note di consegna)
087700150527     C                   EVAL      wTag = 'FTX+DIN'
087800150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087900150527     C                   EVAL      InStringa =
088000150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
088100150527     C                   EVAL      InSepara = '+'
088200150527     C                   EXSR      SR_SPLIT
088300150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
088400150527     C                                     %trim(SkSplitCSV_5(3))
088500150527     C                   ENDIF
088600150527     C*
088700150527     C***
088800150527     C* ...tipo record 'FTX+ACB' (note aggiuntive)
088900150527     C                   EVAL      wTag = 'FTX+ACB'
089000150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089100150527     C                   EVAL      InStringa =
089200150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
089300150527     C                   EVAL      InSepara = '+'
089400150527     C                   EXSR      SR_SPLIT
089500150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
089600150527     C                                     %trim(SkSplitCSV_5(3))
089700150527     C                   ENDIF
089800150609     C*
089900150609     C***
090000150609     C* ...tipo record 'FTX+AAI' (telefono destinatario / note)
090100150609     C                   EVAL      wTag = 'FTX+AAI'
090200150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090300150609     C                   EVAL      InStringa =
090400150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
090500150609     C                   EVAL      InSepara = '+'
090600150609     C                   EXSR      SR_SPLIT
090700150609     C                   MOVEL     *blanks       wSplitCSV_5     256
090800150609     C                   EVAL      wSplitCSV_5 = SkSplitCSV_5(3)
090900150609     C     minu:maiu     XLATE     wSplitCSV_5   wSplitCSV_5                    *Minus -> Maiuscolo
091000150609     C                   IF        %scan('PHONE':wSplitCSV_5) > *zeros
091100150609     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
091200150609     C                                            %scan('PHONE':wSplitCSV_5)+
091300150609     C                                            %len('PHONE')+1))
091400150609     C                   EVAL      VATTRC = 'B'
091500150609     C                   EXSR      WRIVAT
091600150609     C                   ELSE
091700150609     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
091800150609     C                                     %trim(SkSplitCSV_5(3))
091900150609     C                   ENDIF
092000150609     C                   ENDIF
092100141028     C*
092200141028     C***
092300141028     C* ...tipo record 'MEA+WT+AAE' (peso spedizione)
092400151005     C***                EVAL      wTag = 'MEA+WT+AAE'
092500151005     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092600151005     C***                EVAL      InStringa =
092700151005     C***                             %subst(wvindta:%len(%trim(wTag))+2)
092800151005     C***                EVAL      InSepara = ':'
092900151005     C***                EXSR      SR_SPLIT
093000151005     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
093100151005     C***                EXSR      CHKNUM
093200151005     C***                IF        PiNum=*on
093300151005     C***                ADD       PiVal         VABPKB
093400151005     C***                ELSE
093500151005     C***                Z-ADD     1             wRecErr
093600151005     C***                ENDIF
093700151005     C***                ENDIF
093800150810     C*
093900150810     C***
094000150810     C* ...tipo record 'CNT+7' (peso spedizione)
094100150810     C                   EVAL      wTag = 'CNT+7'
094200150810     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
094300150810     C                   EVAL      InStringa =
094400150810     C                                %subst(wvindta:%len(%trim(wTag))+2)
094500150810     C                   EVAL      InSepara = ':'
094600150810     C                   EXSR      SR_SPLIT
094700150810     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
094800150810     C                   EXSR      CHKNUM
094900150810     C                   IF        PiNum=*on
095000151005     C                   Z-ADD     PiVal         VABPKB
095100150810     C                   ELSE
095200150810     C                   Z-ADD     1             wRecErr
095300150810     C                   ENDIF
095400150810     C                   ENDIF
095500150810     C*
095600150810     C***
095700150810     C* ...tipo record 'MEA+WT+AAD' (peso spedizione)
095800151005     C***                EVAL      wTag = 'MEA+WT+AAD'
095900151005     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
096000151005     C***                EVAL      InStringa =
096100151005     C***                             %subst(wvindta:%len(%trim(wTag))+2)
096200151005     C***                EVAL      InSepara = ':'
096300151005     C***                EXSR      SR_SPLIT
096400151005     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
096500151005     C***                EXSR      CHKNUM
096600151005     C***                IF        PiNum=*on
096700151005     C***                ADD       PiVal         VABPKB
096800151005     C***                ELSE
096900151005     C***                Z-ADD     1             wRecErr
097000151005     C***                ENDIF
097100151005     C***                ENDIF
097200141028     C*
097300141028     C***
097400141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
097500141028     C                   EVAL      wTag = 'MEA+VOL'
097600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097700141028     C                   EVAL      InStringa =
097800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
097900141028     C                   EVAL      InSepara = ':'
098000141028     C                   EXSR      SR_SPLIT
098100141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
098200141028     C                   EXSR      CHKNUM
098300141028     C                   IF        PiNum=*on
098400150924     C                   ADD       PiVal         VABVLB
098500141028     C                   ELSE
098600141028     C                   Z-ADD     1             wRecErr
098700141028     C                   ENDIF
098800141028     C                   ENDIF
098900141028     C*
099000141028     C***
099100150810     C* ...tipo record 'GIN+001' (dettagli segnacolli)
099200150810     C                   EVAL      wTag = 'GIN+001'
099300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
099400150526     C                   EVAL      InStringa =
099500150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
099600150526     C                   EVAL      InSepara = '+'
099700150526     C                   EXSR      SR_SPLIT
099800150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
099900150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
100000141028     C                   EVAL      VATTRC = 'E'
100100141028     C                   EXSR      WRIVAT
100200150526     C                   ENDIF
100300150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
100400150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
100500150526     C                   EVAL      VATTRC = 'E'
100600150526     C                   EXSR      WRIVAT
100700150526     C                   ENDIF
100800150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
100900150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
101000150526     C                   EVAL      VATTRC = 'E'
101100150526     C                   EXSR      WRIVAT
101200150526     C                   ENDIF
101300150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
101400150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
101500150526     C                   EVAL      VATTRC = 'E'
101600150526     C                   EXSR      WRIVAT
101700150526     C                   ENDIF
101800150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
101900150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
102000150526     C                   EVAL      VATTRC = 'E'
102100150526     C                   EXSR      WRIVAT
102200150526     C                   ENDIF
102300141028     C                   ENDIF
102400141028     C*
102500141028     C***
102600141028     C* ...tipo record '???' (PDF per packing-list)
102700141028     C                   EVAL      wTag = '???PDF'
102800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102900141028     C*
103000141028     C* ......VATNOT_P
103100141028     C                   if        *in53
103200141028     C*
103300141028     C* Solo se valorizzato RMN
103400141028     C                   if        VABRMA <> *blanks
103500141028     C                   eval      pIn_MaskPDF =
103600141028     C                                     %trim(%subst(wvindta:1292:100))
103700141028     C                   call      'TIS7P2SER'
103800141028     C                   parm      'W'           pIn_Opz           1
103900141028     C                   parm                    tivlrds
104000141028     C                   parm                    EDIVABDS
104100141028     C                   parm      '10'          pIn_CdIdAz        2
104200141028     C                   parm                    pIn_MaskPDF      50
104300141028     C                   parm      *blanks       pOut_Esito        1
104400141028     C                   endif
104500141028     C                   endif
104600141028     C*
104700141028     C                   ENDIF
104800141028     C*
104900141028     C***
105000141028     C* ...tipo record 'UNT' (fine bolla)
105100141028     C                   EVAL      wTag = 'UNT'
105200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
105300150526     C                   IF        *IN90
105400141028     C  N31              ADD       1             §CTROKVB
105500141028     C  N31              exsr      CHKWRI
105600140117     C  N31              WRITE     EDIVAB00
105700150526     C                   ENDIF
105800150526     C                   ENDIF
105900010202     C*
106000000801     C* Ebbene...
106100000801     C                   ADD       1             §CTRMO
106200141028     C                   IF        wRecErr <> *zeros
106300000801     C                   ADD       1             §CTRNO
106400000801     C                   EVAL      vinflg = '2'
106500000801     C                   ENDIF
106600000801     C*
106700000801     C                   ENDSR
106800010202     C*----------------------------------------------------*
106900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
107000010202     C*----------------------------------------------------*
107100020305     C     PREVAT        BEGSR
107200010202     C*
107300140117     C* Compongo il nome del membro da dare al EDIVATWR
107400010202     C                   eval      parmbr = vlrhdl
107500010202     C                   movel     'M'           parmbr
107600060113     C                   eval      parccm = vlrksc
107700010202     C                   eval      paropz = '1'
107800010202     C* Effettuo la chiamata al CLLE preposto
107900140117     C                   call(e)   'TITVEVTC'
108000010202     C                   parm                    parccm
108100010202     C                   parm                    parmbr
108200010202     C                   parm                    paropz
108300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
108400010202     C                   if        %error
108500010202     C                   movel     '1'           chkcall
108600010202     C                   else
108700010202     C                   movel     '0'           chkcall
108800010202     C                   endif
108900010202     C*
109000010202     C                   ENDSR
109100141028     C*----------------------------------------------------*
109200141028     C*  CONTROLLO NUMERICITA' CAMPI
109300141028     C*----------------------------------------------------*
109400141028     C     CHKNUM        BEGSR
109500141028     C*
109600141028     C                   IF        PiDecChr = *blanks
109700141028     C                   EVAL      PiDecChr = CharNUM
109800141028     C                   ENDIF
109900141028     C*
110000141028     C                   callp     UBISNUM_Check(PiStr
110100141028     C                                          :PiDecChr
110200141028     C                                          :PiVal
110300141028     C                                          :PiNum
110400141028     C                                          :PiInt)
110500141028     C*
110600141028     C                   ENDSR
110700141028     C***
110800141028
110900141028
111000141028     C*----------------------------------------------------*
111100141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
111200141028     C*----------------------------------------------------*
111300141028     C     SR_SPLIT      BEGSR
111400141028     C*
111500141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
111600141028     C                   CALL      'XSPLIT2'
111700141028     C                   PARM                    InStringa
111800141028     C                   PARM                    InSepara
111900141028     C                   PARM      '5'           InTypeOut
112000141028     C                   PARM      ''            wSkParam
112100141028     C                   PARM                    OutErrore
112200141028     C                   PARM                    OutMsg
112300141028     C                   MOVEA     wSkParam      SkSplitCSV_5
112400141028     C*
112500141028     C                   ENDSR
112600141028
112700141028
112800141028     C*----------------------------------------------------*
112900141028     C*  SCRITTURA BUFFER EDIVAT
113000141028     C*----------------------------------------------------*
113100141028     C     WRIVAT        BEGSR
113200141028     C*
113300141028     C                   IF        VATNOT <> *blanks
113400141028     C                   EVAL      VATFGS = VABFGS
113500141028     C                   EVAL      VATCCM = VABCCM
113600141028     C                   EVAL      VATLNP = VABLNP
113700141028     C                   EVAL      VATAAS = VABAAS
113800141028     C                   EVAL      VATNRS = VABNRS
113900141028     C                   EVAL      VATNSP = VABNSP
114000141028     C                   EVAL      VATCMR = VABCMR
114100141028     C                   EVAL      VATCNT = VABCNT
114200141028     C                   ADD       1             §CTROKVT
114300141028     C                   WRITE     EDIVAT00
114400141028     C                   ENDIF
114500141028     C*
114600141028     C                   ENDSR
114700000801
114800011113
114900011113     C*----------------------------------------------------*
115000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
115100011113     C*----------------------------------------------------*
115200011113     C     CHKIMPDIV     BEGSR
115300011113     C*
115400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
115500011113     C                   Z-ADD     *zeros        wrkDec            9 9
115600011113     C*
115700011113     C* Come prima cosa effettuo considerazioni sulla divisa
115800011113     C                   IF        vabIAS > *zeros
115900011113     C                   IF        vabVAS <> 'EUR'
116000011113     C                   EVAL      vabVAS =  'ITL'
116100011113     C                   ENDIF
116200011113     C                   ENDIF
116300011113     C*
116400011113     C                   IF        vabCAS > *zeros
116500011113     C                   IF        vabVCA <> 'EUR'
116600011113     C                   EVAL      vabVCA =  'ITL'
116700011113     C                   ENDIF
116800011113     C                   ENDIF
116900011113     C*
117000011113     C                   IF        vabVMD > *zeros
117100020305     C                   IF        vabVAD <> 'EUR'
117200011113     C                   EVAL      vabVAD =  'ITL'
117300011113     C                   ENDIF
117400011113     C                   ENDIF
117500011113     C*
117600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
117700011113     C                   Z-ADD     vabIAS        wrkDec
117800011113     C                   IF        wrkDec > *zeros
117900011113     C                   IF        vabVAS = 'ITL'
118000011113     C                   EVAL      vabIAS = *zeros
118100011113     C                   ENDIF
118200011113     C                   ENDIF
118300011113     C*
118400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
118500011113     C                   Z-ADD     vabCAS        wrkDec
118600011113     C                   IF        wrkDec > *zeros
118700011113     C                   IF        vabVCA = 'ITL'
118800011113     C                   EVAL      vabCAS = *zeros
118900011113     C                   ENDIF
119000011113     C                   ENDIF
119100011113     C*
119200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
119300011113     C                   Z-ADD     vabVMD        wrkDec
119400011113     C                   IF        wrkDec > *zeros
119500011113     C                   IF        vabVAD = 'ITL'
119600011113     C                   EVAL      vabVMD = *zeros
119700011113     C                   ENDIF
119800011113     C                   ENDIF
119900011113     C*
120000011113     C                   ENDSR
120100011113     C***
120200011113
120300011113
120400000801
120500000801
120600990920      /TITLE Invio dei dati al punto operativo.
120700010202     C     invio         BEGSR
120800990920     C*
120900140117     C* 1° invio EDIVAT
121000010201     C                   reset                   dscmz
121100010201     C                   move      vlrpoi        cmzdst
121200140117     C                   eval      cmzfld = 'EDIVATWR'
121300010201     C                   eval      cmzmbd = vlrhdl
121400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
121500021009     C***                if        prmfir = *blanks
121600140117     C                   eval      cmzfla = 'EDIVAT0F'
121700140117     C                   eval      cmzmba = 'EDIVAT0F'
121800021009     C***                else
121900021009     C***                eval      cmzfla = prmfir
122000021009     C***                eval      cmzmba = prmfir
122100021009     C***                endif
122200010201     C                   eval      cmznrr = *zeros
122300020305     C                   move      §ctrokvt      cmznrr
122400021018     C                   eval      cmzlba = vlrfl1
122500010201     C                   call(e)   'TIS711C'
122600010201     C                   parm                    dscmz
122700010201     C                   parm      *blanks       esito
122800010205     C                   if        %error
122900010205     C                             or cmzerr = '1'
123000010205     C                             or esito  = '1'
123100010205     C                   eval      wrkesito = '3'
123200010205     C                   else
123300010201     C*
123400140117     C* 2° invio EDIVAB
123500010201     C                   reset                   dscmz
123600010201     C                   move      vlrpoi        cmzdst
123700010201     C                   eval      cmzfld = vlrfou
123800010201     C                   eval      cmzmbd = vlrhdl
123900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
124000021009     C***                if        prmfir = *blanks
124100140117     C                   eval      cmzfla = 'EDIVAB0F'
124200140117     C                   eval      cmzmba = 'EDIVAB0F'
124300021009     C***                else
124400021009     C***                eval      cmzfla = prmfir
124500021009     C***                eval      cmzmba = prmfir
124600021009     C***                endif
124700010201     C                   eval      cmznrr = *zeros
124800010201     C                   move      §ctrokvb      cmznrr
124900021018     C                   eval      cmzlba = vlrfl1
125000010201     C                   call(e)   'TIS711C'
125100010201     C                   parm                    dscmz
125200010201     C                   parm      *blanks       esito
125300010201     C                   if        %error
125400010201     C                             or cmzerr = '1'
125500010201     C                             or esito  = '1'
125600010201     C                   eval      wrkesito = '3'
125700010201     C                   endif
125800010205     C                   endif
125900990920     C*
126000000613     C                   ENDSR
126100000613     C***
126200070411
126300141028
126400070411     C     *pssr         BEGSR
126500070411     C*
126600070411     C                   if        %open(tivin00r)
126700070411     C                   close     tivin00r
126800070411     C                   endif
126900140117     C                   if        %open(edivabwr)
127000140117     C                   close     edivabwr
127100070411     C                   endif
127200140117     C                   if        %open(edivatwr)
127300140117     C                   close     edivatwr
127400070411     C                   endif
127500070411     C*
127600070411     C* Effettuo la chiamata al CLLE preposto
127700140117     C                   call(e)   'TITVEVTC'
127800070411     C                   parm                    parccm
127900070411     C                   parm                    parmbr
128000070411     C                   parm      '2'           paropz
128100070411     C*
128200070411     C                   eval      wrkesito = '2'
128300070411     C*
128400070411     C                   seton                                        LR
128500070411     C*
128600070411     C                   ENDSR     '*CANCL'
128700070411     C***
128800070411
128900990910
129000000613     C     *inzsr        BEGSR
129100990910     C*
129200990910     C     *entry        plist
129300990920     C                   parm                    tivlrds
129400990921     C                   parm      wrkesito      esito
129500000724     C                   parm                    prmlit
129600000710     C                   parm                    prmfir
129700000613     C*
129800000830     C* CALCOLA LA DATA CORRENTE
129900140117     C                   time                    wn14             14 0
130000140117     C                   movel     wn14          oracor            6 0          *ORA
130100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
130200100722     C                   eval      datcor = %dec(%date() : *ISO)
130300000830     C*
130400000613     C                   ENDSR
130500000613     C***
