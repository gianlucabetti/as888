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
003000141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003100141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003200141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003700141028
003800141028     D*------------------
003900141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000141028     D*------------------
004100141028     D CharNUM         S              1
004200141028     D CharSOS         S              1
004300141028     D posDaDft        S              3  0 INZ(*zeros)
004400141028     D posADft         S              3  0 INZ(*zeros)
004500141028     D wGiroDft        s              1  0 INZ(*zeros)
004600141028
004700141028
004800141028     D*------------------
004900141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005000141028     D*------------------
005100141028     D InStringa       S          65535A   VARYING                              (stringa input)
005200141028     D InSepara        S             10A                                        (separatore)
005300141028     D InTypeOut       S              1A                                        (tipo output)
005400141028     D wSkParam        S          65535A                                        (output)
005500141028     D OutErrore       S              1A                                        (flag errore)
005600141028     D OutMsg          S             80A                                        (messaggio errore)
005700160809     D SkSplitCSV_4    S            128    DIM(512)
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
007700141028
007800141028
007900010201
008000010201
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400000613     C*
008500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008600070719     C*
008700070719     C* Esegue lancio TISI95R solo x chiusura
008800070719     C                   CLEAR                   TISI95DS
008900070719     C                   EVAL      I95TLA = 'C'
009000070719     C                   CALL      'TISI95R'
009100070719     C                   PARM                    TISI95DS
009200000613     C*
009300010202     C* Effettuo la chiamata al CLLE preposto
009400140922     C                   call(e)   'TITVEVTC'
009500140922     C                   parm                    parccm
009600140922     C                   parm                    parmbr
009700140922     C                   parm      '2'           paropz
009800170731     C*
009900170731     C                   if        *in53
010000170731     C                   call      'TIS7P2SER'
010100170731     C                   parm      'C'           pIn_Opz           1
010200170731     C                   parm                    tivlrds
010300170731     C                   parm                    EDIVABDS
010400170731     C                   parm      *blanks       pIn_CdIdAz        2
010500170731     C                   parm      *blanks       pIn_MaskPDF      50
010600170731     C                   parm      *blanks       pOut_Esito        1
010700170731     C                   endif
010800000616     C*
010900010201     C                   seton                                        LR
011000990908
011100000801
011200910830     C*--------------------------------------------------------
011300140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011400910830     C*--------------------------------------------------------
011500040526     C     RWFILE        BEGSR
011600990910     C*
011700990914     C                   if        not %open(tivin00r)
011800990908     C                   open      tivin00r
011900990914     C                   endif
012000140117     C                   if        not %open(edivabwr)
012100140117     C                   open      edivabwr
012200990914     C                   endif
012300141028     C*
012400140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012500020305     C                   exsr      prevat
012600010201     C*
012700010202     C                   if        chkcall = '0'
012800010202     C*
012900140117     C                   if        not %open(edivatwr)
013000140117     C                   open      edivatwr
013100010201     C                   endif
013200990910     C*
013300010201     C                   clear                   §CTROKVB          5 0
013400020305     C                   clear                   §CTROKVT          5 0
013500000801     C                   clear                   §CTRMO            5 0
013600000801     C                   clear                   §CTRNO            5 0
013700141028     C*
013800141028     C* Inizializzazioni fuori ciclo
013900141028     C                   EXSR      INZVAR
014000140117     C*
014100140902     C                   if        *in53
014200140117     C                   call      'TIS7P2SER'
014300140117     C                   parm      'O'           pIn_Opz           1
014400140117     C                   parm                    tivlrds
014500140117     C                   parm                    EDIVABDS
014600140117     C                   parm      *blanks       pIn_CdIdAz        2
014700140117     C                   parm      *blanks       pIn_MaskPDF      50
014800140117     C                   parm      *blanks       pOut_Esito        1
014900140902     C                   endif
015000130708     C*
015100921023     C                   DO        *HIVAL
015200990913     C*
015300990915     C                   READ      tivin00r                               70
015400040910     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600000801     C*
015700000801     C                   if        *in70 = *off
015800000801     C                             and
015900000801     C                             (vinflg = *blanks
016000000801     C                              or vinflg = '0'
016100000801     C                              or vinflg = '2')
016200000801     C*
016300000801     C                   clear                   vinmsg
016400141028     C*
016500141028     C* "normalizzo" il dato di input
016600141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
016700141028     C                                             %len(%trim(vindta))-1))
016800160209     C*
016900160209     C* Elimino le "duple di controllo EDIFACT"
017000160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017100160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
017200160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
017300160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
017400160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
017500040910     C*
017600040910     C* Eseguo routine d traduzione
017700040910     C                   exsr      impvabvat
017800040802     C*
017900010305     C                   endif
018000000905     C*
018100000905     C                   else
018200000905     C                   eval      vinflg = '1'
018300000905     C                   endif
018400000905     C*
018500000905     C  N70              update    tivin000
018600000905     C*
018700991022     C  N70              ENDdo
018800100722     C*
018900100722     C* Scarico i buffer testata ancora "in canna"
019000140922     C*
019100141028     C***  N31              ADD       1             §CTROKVB
019200141028     C***  N31              exsr      CHKWRI
019300141028     C***  N31              WRITE     EDIVAB00
019400010202     C*
019500010202     C                   endif
019600990910
019700990910     C* Se non ci sono record con errori ...
019800000710     C                   if        §ctrno = 0
019900990910     C* ... restituisco esito OK.
020000990921     C                   eval      wrkesito = '0'
020100990910     C                   else
020200010201     C                   if        §ctrokvb > 0
020300990921     C                   eval      wrkesito = '1'
020400000710     C                   else
020500000710     C                   eval      wrkesito = '2'
020600990910     C                   endif
020700000710     C                   endif
020800990910     C*
020900990914     C                   if        %open(tivin00r)
021000990908     C                   close     tivin00r
021100990914     C                   endif
021200140117     C                   if        %open(edivabwr)
021300140117     C                   close     edivabwr
021400990914     C                   endif
021500140117     C                   if        %open(edivatwr)
021600140117     C                   close     edivatwr
021700010201     C                   endif
021800990910     C*
021900010201     C                   if        §ctrokvb > 0
022000000724     C                             and vlrpoi <> *zeros
022100010202     C                   exsr      invio
022200990920     C                   endif
022300990920     C*
022400910830     C                   ENDSR
022500000613     C***
022600140922
022700140922     C*----------------------------------------------------*
022800140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
022900140922     C*----------------------------------------------------*
023000140922     C     CHKWRI        BEGSR
023100140922     C*
023200141028     C                   if        VABRMN = *zeros
023300141028     C                   eval      VABRMN = VABNSP
023400141028     C                   endif
023500151102     C*
023600151102     C                   if        VABRMA = *blanks
023700151102     C                   eval      VABRMA = savCMR
023800151102     C                   endif
023900160803     C*
024000160803     C                   eval      wNOTE  = %trim(wNOTE)
024100160803     C                   eval      VABNOT = %subst(wNOTE:1:35)
024200160803     C                   eval      VABNT2 = %subst(wNOTE:36:35)
024300141028     C*
024400141028     C                   if        VABPRD = *blanks
024500141028     C* ......VABPRD
024600141028     C* Reperisco la provincia dal CAP e dalla località
024700141028     C                   IF        VABLOD <> *blanks AND
024800141028     C                             VABCAD <> *blanks AND
024900141028     C                             VABNZD  = *blanks
025000141028     C                   CLEAR                   TISI95DS
025100141028     C                   EVAL      I95TCN = '3'
025200141028     C                   Z-ADD     datcor        I95DAT
025300141028     C                   EVAL      I95CAP = VABCAD
025400141028     C                   EVAL      I95LOC = VABLOD
025500141028     C                   CALL      'TISI95R'
025600141028     C                   PARM                    TISI95DS
025700141028     C                   EVAL      VABPRD = O95PRV
025800141028     C                   ENDIF
025900141028     C                   endif
026000140922     C*
026100140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
026200141028     C                   IF        wFlgCAS <> '0'
026300140922     C                   IF        VABCBO = '1'
026400140922     C                   EVAL      VABCBO = '4'
026500140922     C                   ELSE
026600140922     C                   EVAL      VABCBO = '6'
026700140922     C                   ENDIF
026800140922     C                   ENDIF
026900140922     C*
027000140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027100141028     C                   EXSR      CHKIMPDIV
027200140922     C*
027300140922     C                   ENDSR
027400141028     C*----------------------------------------------------*
027500141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027600141028     C*----------------------------------------------------*
027700141028     C     INZVAR        BEGSR
027800141028     C*
027900141028     C* Inizializzo variabili di wrk
028000141028     C                   Z-ADD     *zeros        Num5_0            5 0
028100141028     C                   MOVEL     '0'           wFlgCAS
028200141028     C                   MOVEL     *blanks       wVATNOT_A
028300141028     C                   MOVEL     *blanks       wVATNOT_B
028400141028     C                   MOVEL     *blanks       wVATNOT_E
028500141028     C                   MOVEL     *blanks       wVATNOT_I
028600141028     C                   MOVEL     *blanks       wVATNOT_J
028700141028     C                   MOVEL     *blanks       wVATNOT_S
028800141028     C                   MOVEL     *blanks       wVATNOT_P
028900160809     C                   CLEAR                   SkSplitCSV_4
029000141028     C                   CLEAR                   SkSplitCSV_5
029100141028     C*
029200141028     C* Reimposto i valori di default
029300141028     C                   EXSR      DEFCAM
029400141028     C*
029500141028     C                   ENDSR
029600141028     C*----------------------------------------------------*
029700141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029800141028     C*----------------------------------------------------*
029900141028     C     DEFCAM        BEGSR
030000141028     C*
030100141028     C                   CLEAR                   EDIVAB00
030200141028     C                   CLEAR                   EDIVAT00
030300141028     C*
030400141028     C* Imposto i valori bolla di default
030500141028     C                   EVAL      VABCTR = 000
030600141028     C                   EVAL      VABCBO = '1'
030700141104     C                   EVAL      VABTSP = 'C'
030800141028     C*
030900141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
031000141028     C* e delimitatore testo.
031100141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
031200141028     C*
031300141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031400141028     C                   EVAL      CharSOS = CharNUM
031500141028     C*
031600141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
031700141028     C                   SETOFF                                       53
031800141028     C                   SELECT
031900141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
032000141028     C                   SETON                                        53
032100141028     C                   ENDSL
032200141028     C*
032300141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032400141028     C                   EVAL      posDaDft = 1
032500141028     C                   EVAL      posADft  = 0
032600141028     C                   EVAL      wGiroDft = 0
032700141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032800141028     C                             posDaDft > 0
032900141028     C*
033000141028     C* Gestisco il 1° giro
033100141028     C                   IF        wGiroDft = 0
033200141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
033300141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033400141028     C* Incremento il contatore dei "giri"
033500141028     C                   EVAL      wGiroDft = 1
033600141028     C                   ELSE
033700141028     C                   EVAL      posDaDft = posADft
033800141028     C                   ENDIF
033900141028     C* Eseguo lo scan x trovare la fine del campo corrente
034000141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
034100141028     C*
034200141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
034300141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
034400141028     C                   IF        posDaDft > 0 AND
034500141028     C                             posADft  > 0
034600141028     C* NCL
034700141028     C                   IF        %subst(
034800141028     C                             %subst(vlrppt:posDaDft+1:
034900141028     C                             posADft-posDaDft-1):1:3)
035000141028     C                             = 'NCL'
035100141028     C                   EVAL      PiStr=%trim(%subst(
035200141028     C                             %subst(vlrppt:posDaDft+1:
035300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035400141028     C                   EXSR      CHKNUM
035500141028     C                   IF        PiInt=*on
035600141028     C                   Z-ADD     PiVal         VABNCL
035700141028     C                   ENDIF
035800141028     C                   ENDIF
035900141028     C* CCM
036000141028     C                   IF        %subst(
036100141028     C                             %subst(vlrppt:posDaDft+1:
036200141028     C                             posADft-posDaDft-1):1:3)
036300141028     C                             = 'CCM'
036400141028     C                   EVAL      PiStr=%trim(%subst(
036500141028     C                             %subst(vlrppt:posDaDft+1:
036600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036700141028     C                   EXSR      CHKNUM
036800141028     C                   IF        PiInt=*on
036900141028     C                   Z-ADD     PiVal         VABCCM
037000141028     C                   ENDIF
037100141028     C                   ENDIF
037200141028     C* LNP
037300141028     C                   IF        %subst(
037400141028     C                             %subst(vlrppt:posDaDft+1:
037500141028     C                             posADft-posDaDft-1):1:3)
037600141028     C                             = 'LNP'
037700141028     C                   EVAL      PiStr=%trim(%subst(
037800141028     C                             %subst(vlrppt:posDaDft+1:
037900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038000141028     C                   EXSR      CHKNUM
038100141028     C                   IF        PiInt=*on
038200141028     C                   Z-ADD     PiVal         VABLNP
038300141028     C                   ENDIF
038400141028     C                   ENDIF
038500141028     C* NRS
038600141028     C                   IF        %subst(
038700141028     C                             %subst(vlrppt:posDaDft+1:
038800141028     C                             posADft-posDaDft-1):1:3)
038900141028     C                             = 'NRS'
039000141028     C                   EVAL      PiStr=%trim(%subst(
039100141028     C                             %subst(vlrppt:posDaDft+1:
039200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039300141028     C                   EXSR      CHKNUM
039400141028     C                   IF        PiInt=*on
039500141028     C                   Z-ADD     PiVal         VABNRS
039600141028     C                   ENDIF
039700141028     C                   ENDIF
039800141028     C* CTR
039900141028     C                   IF        %subst(
040000141028     C                             %subst(vlrppt:posDaDft+1:
040100141028     C                             posADft-posDaDft-1):1:3)
040200141028     C                             = 'CTR'
040300141028     C                   EVAL      PiStr=%trim(%subst(
040400141028     C                             %subst(vlrppt:posDaDft+1:
040500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040600141028     C                   EXSR      CHKNUM
040700141028     C                   IF        PiInt=*on
040800141028     C                   Z-ADD     PiVal         VABCTR
040900141028     C                   ENDIF
041000141028     C                   ENDIF
041100141028     C* PKB
041200141028     C                   IF        %subst(
041300141028     C                             %subst(vlrppt:posDaDft+1:
041400141028     C                             posADft-posDaDft-1):1:3)
041500141028     C                             = 'PKB'
041600141028     C                   EVAL      PiStr=%trim(%subst(
041700141028     C                             %subst(vlrppt:posDaDft+1:
041800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041900141028     C                   EXSR      CHKNUM
042000141028     C                   IF        PiNum=*on
042100141028     C                   Z-ADD     PiVal         VABPKB
042200141028     C                   ENDIF
042300141028     C                   ENDIF
042400141028     C* VLB
042500141028     C                   IF        %subst(
042600141028     C                             %subst(vlrppt:posDaDft+1:
042700141028     C                             posADft-posDaDft-1):1:3)
042800141028     C                             = 'VLB'
042900141028     C                   EVAL      PiStr=%trim(%subst(
043000141028     C                             %subst(vlrppt:posDaDft+1:
043100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043200141028     C                   EXSR      CHKNUM
043300141028     C                   IF        PiNum=*on
043400141028     C                   Z-ADD     PiVal         VABVLB
043500141028     C                   ENDIF
043600141028     C                   ENDIF
043700141028     C* QFT
043800141028     C                   IF        %subst(
043900141028     C                             %subst(vlrppt:posDaDft+1:
044000141028     C                             posADft-posDaDft-1):1:3)
044100141028     C                             = 'QFT'
044200141028     C                   EVAL      PiStr=%trim(%subst(
044300141028     C                             %subst(vlrppt:posDaDft+1:
044400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500141028     C                   EXSR      CHKNUM
044600141028     C                   IF        PiNum=*on
044700141028     C                   Z-ADD     PiVal         VABQFT
044800141028     C                   ENDIF
044900141028     C                   ENDIF
045000141028     C* CBO
045100141028     C                   IF        %subst(
045200141028     C                             %subst(vlrppt:posDaDft+1:
045300141028     C                             posADft-posDaDft-1):1:3)
045400141028     C                             = 'CBO'
045500141028     C                   EVAL      VABCBO=%trim(%subst(
045600141028     C                             %subst(vlrppt:posDaDft+1:
045700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800141028     C                   ENDIF
045900141028     C* TSP
046000141028     C                   IF        %subst(
046100141028     C                             %subst(vlrppt:posDaDft+1:
046200141028     C                             posADft-posDaDft-1):1:3)
046300141028     C                             = 'TSP'
046400141028     C                   EVAL      VABTSP=%trim(%subst(
046500141028     C                             %subst(vlrppt:posDaDft+1:
046600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700141028     C                   ENDIF
046800141028     C* VAS
046900141028     C                   IF        %subst(
047000141028     C                             %subst(vlrppt:posDaDft+1:
047100141028     C                             posADft-posDaDft-1):1:3)
047200141028     C                             = 'VAS'
047300141028     C                   EVAL      VABVAS=%trim(%subst(
047400141028     C                             %subst(vlrppt:posDaDft+1:
047500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600141028     C                   ENDIF
047700141028     C* VCA
047800141028     C                   IF        %subst(
047900141028     C                             %subst(vlrppt:posDaDft+1:
048000141028     C                             posADft-posDaDft-1):1:3)
048100141028     C                             = 'VCA'
048200141028     C                   EVAL      VABVCA=%trim(%subst(
048300141028     C                             %subst(vlrppt:posDaDft+1:
048400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500141028     C                   ENDIF
048600141028     C* TIC
048700141028     C                   IF        %subst(
048800141028     C                             %subst(vlrppt:posDaDft+1:
048900141028     C                             posADft-posDaDft-1):1:3)
049000141028     C                             = 'TIC'
049100141028     C                   EVAL      VABTIC=%trim(%subst(
049200141028     C                             %subst(vlrppt:posDaDft+1:
049300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400141028     C                   ENDIF
049500141028     C* GCA
049600141028     C                   IF        %subst(
049700141028     C                             %subst(vlrppt:posDaDft+1:
049800141028     C                             posADft-posDaDft-1):1:3)
049900141028     C                             = 'GCA'
050000141028     C                   EVAL      VABGCA=%trim(%subst(
050100141028     C                             %subst(vlrppt:posDaDft+1:
050200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300141028     C                   ENDIF
050400141028     C* CTM
050500141028     C                   IF        %subst(
050600141028     C                             %subst(vlrppt:posDaDft+1:
050700141028     C                             posADft-posDaDft-1):1:3)
050800141028     C                             = 'CTM'
050900141028     C                   EVAL      VABCTM=%trim(%subst(
051000141028     C                             %subst(vlrppt:posDaDft+1:
051100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200141028     C                   ENDIF
051300141028     C* FFD
051400141028     C                   IF        %subst(
051500141028     C                             %subst(vlrppt:posDaDft+1:
051600141028     C                             posADft-posDaDft-1):1:3)
051700141028     C                             = 'FFD'
051800141028     C                   EVAL      VABFFD=%trim(%subst(
051900141028     C                             %subst(vlrppt:posDaDft+1:
052000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100141028     C                   ENDIF
052200141028     C* VAD
052300141028     C                   IF        %subst(
052400141028     C                             %subst(vlrppt:posDaDft+1:
052500141028     C                             posADft-posDaDft-1):1:3)
052600141028     C                             = 'VAD'
052700141028     C                   EVAL      VABVAD=%trim(%subst(
052800141028     C                             %subst(vlrppt:posDaDft+1:
052900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000141028     C                   ENDIF
053100141028     C* GMA
053200141028     C                   IF        %subst(
053300141028     C                             %subst(vlrppt:posDaDft+1:
053400141028     C                             posADft-posDaDft-1):1:3)
053500141028     C                             = 'GMA'
053600141028     C                   EVAL      VABGMA=%trim(%subst(
053700141028     C                             %subst(vlrppt:posDaDft+1:
053800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900141028     C                   ENDIF
054000141028     C* GGA
054100141028     C                   IF        %subst(
054200141028     C                             %subst(vlrppt:posDaDft+1:
054300141028     C                             posADft-posDaDft-1):1:3)
054400141028     C                             = 'GGA'
054500141028     C                   EVAL      VABGGA=%trim(%subst(
054600141028     C                             %subst(vlrppt:posDaDft+1:
054700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800141028     C                   ENDIF
054900141028     C* GVA
055000141028     C                   IF        %subst(
055100141028     C                             %subst(vlrppt:posDaDft+1:
055200141028     C                             posADft-posDaDft-1):1:3)
055300141028     C                             = 'GVA'
055400141028     C                   EVAL      VABGVA=%trim(%subst(
055500141028     C                             %subst(vlrppt:posDaDft+1:
055600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700141028     C                   ENDIF
055800141028     C* TC1
055900141028     C                   IF        %subst(
056000141028     C                             %subst(vlrppt:posDaDft+1:
056100141028     C                             posADft-posDaDft-1):1:3)
056200141028     C                             = 'TC1'
056300141028     C                   EVAL      VABTC1=%trim(%subst(
056400141028     C                             %subst(vlrppt:posDaDft+1:
056500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600141028     C                   ENDIF
056700141028     C* TC2
056800141028     C                   IF        %subst(
056900141028     C                             %subst(vlrppt:posDaDft+1:
057000141028     C                             posADft-posDaDft-1):1:3)
057100141028     C                             = 'TC2'
057200141028     C                   EVAL      VABTC2=%trim(%subst(
057300141028     C                             %subst(vlrppt:posDaDft+1:
057400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500141028     C                   ENDIF
057600141028     C* VATTRC
057700141028     C                   IF        %subst(
057800141028     C                             %subst(vlrppt:posDaDft+1:
057900141028     C                             posADft-posDaDft-1):1:3)
058000141028     C                             = 'TRC'
058100141028     C                   EVAL      VATTRC=%trim(%subst(
058200141028     C                             %subst(vlrppt:posDaDft+1:
058300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400141028     C                   ENDIF
058500141028     C* ...
058600141028     C                   ENDIF
058700141028     C                   ENDDO
058800141028     C*
058900141028     C                   ENDSR
059000000801     C*----------------------------------------------------*
059100140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
059200000801     C*----------------------------------------------------*
059300040910     C     IMPVABVAT     BEGSR
059400040910     C*
059500040910     C* Traduzione relativa ai tipi record del file del cliente
059600040910     C*
059700071210     C*
059800071210     C***
059900141028     C* ...tipo record 'UNB' (info CMR)
060000141028     C                   EVAL      wTag = 'UNB'
060100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
060200141028     C                   MOVEL     *blanks       savCMR           35
060300141028     C                   EVAL      InStringa =
060400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
060500141028     C                   EVAL      InSepara = '+'
060600160809     C                   EXSR      SR_SPLIT5
060700160809     C                   EVAL      savCMR = %trim(SkSplitCSV_5(4)) + ' '+
060800140922     C                                      %char(datcor) + ' ' + %char(oracor)
060900130620     C                   ENDIF
061000141028     C*
061100141028     C***
061200141028     C* ...tipo record 'UNH' (rottuta di codice spedizione)
061300141028     C                   EVAL      wTag = 'UNH'
061400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061500141028     C                   SETOFF                                       31
061600141028     C                   eval      vinflg = '1'
061700141028     C* ......inizializzazioni iniziali e formati record file Bartolini
061800141028     C                   EXSR      INZVAR
061900160803     C                   MOVEL     *blanks       wNOTE            70
062000141028     C* ......valorizzazione campi fissi
062100141028     C                   MOVEL     datcor        VABAAS
062200141028     C                   MOVE      datcor        VABMGS
062300141028     C                   MOVE(P)   vlrpoi        VABFGS
062400141028     C                   EVAL      VABCMR = savCMR
062500141028     C                   EVAL      VABDCM = datcor
062600141028     C                   EVAL      VABDTS = datcor
062700141028     C                   EVAL      VABCNT = 1
062800141028     C* ......NSP => Stacco un numeratore da AZNUM
062900141028     C                   clear                   TRUL33DS
063000141028     C                   eval      I33OPE = *zeros
063100141028     C                   eval      I33CNU = 302
063200141028     C                   eval      I33NUM = 1
063300141028     C                   movel     TRUL33DS      KPJBU
063400141028     C                   call      'TRUL33R'
063500141028     C                   parm                    KPJBA
063600141028     C                   movel     KPJBU         TRUL33DS
063700141028     C                   if        O33ERR = *zeros
063800141028     C                   z-add     O33NRF        VABNSP
063900141028     C                   else
064000141028     C                   Z-ADD     1             wRecErr
064100141028     C                   SETON                                        31
064200141028     C                   EVAL      vinmsg = %trimr(vinmsg)
064300141028     C                             + ' ' + 'VABNSP VATNSP'
064400141028     C                   endif
064500141028     C                   ENDIF
064600160924     C*
064700160924     C***
064800160924     C* ...tipo record 'FTX+TRA' (tipo servizio)
064900160924     C                   SETOFF                                       60
065000160924     C                   EVAL      wTag = 'FTX+TRA'
065100160924     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
065200160924     C                   EVAL      InStringa =
065300160924     C                                %subst(wvindta:%len(%trim(wTag))+2)
065400160924     C                   EVAL      InSepara = '+'
065500160924     C                   EXSR      SR_SPLIT5
065600160924     C                   IF        %trim(SkSplitCSV_5(3)) = 'LAUNCH - STREET'
065700160924     C                   SETON                                        60
065800160924     C                   ENDIF
065900160924     C                   ENDIF
066000160803     C*
066100160803     C***
066200160924     C* ...tipo record 'DTM+63' (data consegna richiesta)
066300160924     C                   EVAL      wTag = 'DTM+63'
066400160803     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066500160803     C                   EVAL      PiStr=%subst(
066600160803     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
066700160803     C                   EXSR      CHKNUM
066800160803     C                   IF        PiInt=*on
066900160924     C   60              Z-ADD     PiVal         VABDCR
067000160803     C                   ELSE
067100160803     C                   Z-ADD     1             wRecErr
067200160803     C                   ENDIF
067300160803     C                   ENDIF
067400160803     C*
067500160803     C***
067600160809     C* ...tipo record 'BGM+785' (riferimento spedizione)
067700160809     C                   EVAL      wTag = 'BGM+785'
067800160809     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067900160809     C                   EVAL      InStringa =
068000160809     C                                %subst(wvindta:%len(%trim(wTag))+2)
068100160809     C                   EVAL      InSepara = '+'
068200160809     C                   EXSR      SR_SPLIT5
068300160809     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
068400160803     C                   ENDIF
068500151124     C*
068600151124     C***
068700160809     C* ...tipo record 'RFF+CU' (riferimenti spedizione)
068800160809     C                   EVAL      wTag = 'RFF+CU'
068900151124     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069000160229     C                   EVAL      InStringa =
069100160229     C                                %subst(wvindta:%len(%trim(wTag))+2)
069200160229     C                   EVAL      InSepara = '+'
069300160809     C                   EXSR      SR_SPLIT5
069400151124     C* RMN
069500160803     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
069600151124     C                   EXSR      CHKNUM
069700151124     C                   IF        PiInt=*on
069800151124     C                   Z-ADD     PiVal         VABRMN
069900151124     C                   ELSE
070000151124     C                   Z-ADD     1             wRecErr
070100151124     C                   ENDIF
070200151124     C                   ENDIF
070300160910     C*
070400160910     C***
070500160910     C* ...tipo record 'NAD+CZ (codice mittente spedizione)
070600160910     C                   EVAL      wTag = 'NAD+CZ'
070700160910     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070800160910     C                   EVAL      InStringa =
070900160910     C                                %subst(wvindta:%len(%trim(wTag))+2)
071000160910     C                   EVAL      InSepara = '+'
071100160910     C                   EXSR      SR_SPLIT5
071200160910     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
071300160910     C                   EXSR      CHKNUM
071400160910     C                   IF        PiNum=*on
071500160910     C                   Z-ADD     PiVal         VABCCM
071600160910     C                   ELSE
071700160910     C                   Z-ADD     1             wRecErr
071800160910     C                   ENDIF
071900160910     C* Eseguo forzature a livello di codice mittente
072000160910     C                   SELECT
072100160910     C                   WHEN      VABCCM = 0896081                             * Traffico PARCEL
072200160910     C                   EVAL      VABLNP = 089
072300160910     C                   EVAL      VABCTR = 300
072400160910     C                   ENDSL
072500160910     C                   ENDIF
072600141028     C*
072700141028     C***
072800141028     C* ...tipo record 'NAD+CN' (destinatario)
072900141028     C                   EVAL      wTag = 'NAD+CN'
073000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073100141028     C                   EVAL      InStringa =
073200141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
073300141028     C                   EVAL      InSepara = '+'
073400160809     C                   EXSR      SR_SPLIT5
073500141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
073600141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
073700141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
073800141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
073900141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
074000141028     C                   IF        VABNZD = 'I'   OR
074100141028     C                             VABNZD = 'IT'  OR
074200141028     C                             VABNZD = 'ITA'
074300141028     C                   EVAL      VABNZD = *blanks
074400141028     C                   ENDIF
074500141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
074600141028     C                   EVAL      InSepara = ':'
074700160809     C                   EXSR      SR_SPLIT5
074800141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
074900141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
075000160809     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
075100160809     C                                      %trim(SkSplitCSV_5(4))+
075200160809     C                                      %trim(SkSplitCSV_5(5))
075300141028     C                   ENDIF
075400141028     C*
075500160809     C***
075600160809     C* ...tipo record 'CNT+11' (colli)
075700160809     C                   EVAL      wTag = 'CNT+11'
075800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075900141028     C                   EVAL      InStringa =
076000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
076100160809     C                   EVAL      InSepara = '+'
076200160809     C                   EXSR      SR_SPLIT5
076300160809     C                   EVAL      InStringa = %trim(SkSplitCSV_5(1))
076400160201     C                   EVAL      InSepara = ':'
076500160809     C                   EXSR      SR_SPLIT5
076600160201     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
076700141028     C                   EXSR      CHKNUM
076800141028     C                   IF        PiInt=*on
076900141028     C                   Z-ADD     PiVal         VABNCL
077000141028     C                   ELSE
077100141028     C                   Z-ADD     1             wRecErr
077200141028     C                   ENDIF
077300141028     C                   ENDIF
077400160809     C*
077500160809     C***
077600160809     C* ...tipo record 'MOA+43' (valore merce dichiarato)
077700160809     C                   EVAL      wTag = 'MOA+43'
077800160809     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077900160809     C                   EVAL      InStringa =
078000160809     C                                %subst(wvindta:%len(%trim(wTag))+2)
078100160809     C                   EVAL      InSepara = ':'
078200160809     C                   EXSR      SR_SPLIT5
078300160809     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
078400160809     C                   EXSR      CHKNUM
078500160809     C                   IF        PiNum=*on
078600160809     C                   Z-ADD     PiVal         VABVMD
078700160809     C                   ELSE
078800160809     C                   Z-ADD     1             wRecErr
078900160809     C                   ENDIF
079000160809     C                   ENDIF
079100141028     C*
079200141028     C***
079300141028     C* ...tipo record 'FTX+AAA' (natura merce)
079400141028     C                   EVAL      wTag = 'FTX+AAA'
079500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079600141028     C                   EVAL      InStringa =
079700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079800141028     C                   EVAL      InSepara = '+'
079900160809     C                   EXSR      SR_SPLIT5
080000141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
080100141028     C                   ENDIF
080200160809     C*
080300160809     C***
080400160809     C* ...tipo record 'CNT+7' (peso spedizione)
080500160809     C                   EVAL      wTag = 'CNT+7'
080600160809     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080700160809     C                   EVAL      InStringa =
080800160809     C                                %subst(wvindta:%len(%trim(wTag))+2)
080900160809     C                   EVAL      InSepara = ':'
081000160809     C                   EXSR      SR_SPLIT5
081100160809     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
081200160809     C                   EXSR      CHKNUM
081300160809     C                   IF        PiNum=*on
081400160809     C                   Z-ADD     PiVal         VABPKB
081500160809     C                   ELSE
081600160809     C                   Z-ADD     1             wRecErr
081700160809     C                   ENDIF
081800160809     C                   ENDIF
081900160809     C*
082000160809     C***
082100160809     C* ...tipo record 'CNT+15' (volume spedizione)
082200160809     C                   EVAL      wTag = 'CNT+15'
082300160809     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082400160809     C                   EVAL      InStringa =
082500160809     C                                %subst(wvindta:%len(%trim(wTag))+2)
082600160809     C                   EVAL      InSepara = ':'
082700160809     C                   EXSR      SR_SPLIT5
082800160809     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
082900160809     C                   EXSR      CHKNUM
083000160809     C                   IF        PiNum=*on
083100160809     C                   IF        %trim(SkSplitCSV_5(2)) = 'MTQ'
083200160809     C                   EVAL      PiVal = PiVal / 1000000
083300160809     C                   ENDIF
083400160809     C                   Z-ADD     PiVal         VABVLB
083500160809     C                   ELSE
083600160809     C                   Z-ADD     1             wRecErr
083700160809     C                   ENDIF
083800160809     C                   ENDIF
083900141028     C*
084000141028     C***
084100160809     C* ...tipo record 'GIN+VZ' (dettagli segnacolli)
084200160809     C                   EVAL      wTag = 'GIN+VZ'
084300151218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
084400151218     C                   EVAL      InStringa =
084500151218     C                                %subst(wvindta:%len(%trim(wTag))+2)
084600151218     C                   EVAL      InSepara = '+'
084700160809     C                   EXSR      SR_SPLIT4
084800160809     C                   EVAL      InStringa = %trim(SkSplitCSV_4(1))
084900160809     C                   EVAL      InSepara = ':'
085000160809     C                   EXSR      SR_SPLIT5
085100160809     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
085200160809     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
085300151218     C                   EVAL      VATTRC = 'E'
085400151218     C                   EXSR      WRIVAT
085500151218     C                   ENDIF
085600160809     C*
085700160809     C                   EVAL      InStringa = %trim(SkSplitCSV_4(2))
085800160809     C                   EVAL      InSepara = ':'
085900160809     C                   EXSR      SR_SPLIT5
086000160809     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
086100160809     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
086200160809     C                   EVAL      VATTRC = 'E'
086300160809     C                   EXSR      WRIVAT
086400160809     C                   ENDIF
086500160809     C*
086600160809     C                   EVAL      InStringa = %trim(SkSplitCSV_4(3))
086700160809     C                   EVAL      InSepara = ':'
086800160809     C                   EXSR      SR_SPLIT5
086900160809     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
087000160809     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
087100160809     C                   EVAL      VATTRC = 'E'
087200160809     C                   EXSR      WRIVAT
087300160809     C                   ENDIF
087400160809     C*
087500160809     C                   EVAL      InStringa = %trim(SkSplitCSV_4(4))
087600160809     C                   EVAL      InSepara = ':'
087700160809     C                   EXSR      SR_SPLIT5
087800160809     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
087900160809     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
088000160809     C                   EVAL      VATTRC = 'E'
088100160809     C                   EXSR      WRIVAT
088200160809     C                   ENDIF
088300160809     C*
088400160809     C                   EVAL      InStringa = %trim(SkSplitCSV_4(5))
088500160809     C                   EVAL      InSepara = ':'
088600160809     C                   EXSR      SR_SPLIT5
088700160809     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
088800160809     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
088900160809     C                   EVAL      VATTRC = 'E'
089000160809     C                   EXSR      WRIVAT
089100160809     C                   ENDIF
089200160809     C*
089300151218     C                   ENDIF
089400141028     C*
089500141028     C***
089600141028     C* ...tipo record '???' (PDF per packing-list)
089700141028     C                   EVAL      wTag = '???PDF'
089800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089900141028     C*
090000141028     C* ......VATNOT_P
090100141028     C                   if        *in53
090200141028     C*
090300141028     C* Solo se valorizzato RMN
090400141028     C                   if        VABRMA <> *blanks
090500141028     C                   eval      pIn_MaskPDF =
090600141028     C                                     %trim(%subst(wvindta:1292:100))
090700141028     C                   call      'TIS7P2SER'
090800141028     C                   parm      'W'           pIn_Opz           1
090900141028     C                   parm                    tivlrds
091000141028     C                   parm                    EDIVABDS
091100141028     C                   parm      '10'          pIn_CdIdAz        2
091200141028     C                   parm                    pIn_MaskPDF      50
091300141028     C                   parm      *blanks       pOut_Esito        1
091400141028     C                   endif
091500141028     C                   endif
091600141028     C*
091700141028     C                   ENDIF
091800141028     C*
091900141028     C***
092000141028     C* ...tipo record 'UNT' (fine bolla)
092100141028     C                   EVAL      wTag = 'UNT'
092200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092300141028     C  N31              ADD       1             §CTROKVB
092400141028     C  N31              exsr      CHKWRI
092500140117     C  N31              WRITE     EDIVAB00
092600100722     C                   endif
092700010202     C*
092800000801     C* Ebbene...
092900000801     C                   ADD       1             §CTRMO
093000141028     C                   IF        wRecErr <> *zeros
093100000801     C                   ADD       1             §CTRNO
093200000801     C                   EVAL      vinflg = '2'
093300000801     C                   ENDIF
093400000801     C*
093500000801     C                   ENDSR
093600010202     C*----------------------------------------------------*
093700140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
093800010202     C*----------------------------------------------------*
093900020305     C     PREVAT        BEGSR
094000010202     C*
094100140117     C* Compongo il nome del membro da dare al EDIVATWR
094200010202     C                   eval      parmbr = vlrhdl
094300010202     C                   movel     'M'           parmbr
094400060113     C                   eval      parccm = vlrksc
094500010202     C                   eval      paropz = '1'
094600010202     C* Effettuo la chiamata al CLLE preposto
094700140117     C                   call(e)   'TITVEVTC'
094800010202     C                   parm                    parccm
094900010202     C                   parm                    parmbr
095000010202     C                   parm                    paropz
095100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
095200010202     C                   if        %error
095300010202     C                   movel     '1'           chkcall
095400010202     C                   else
095500010202     C                   movel     '0'           chkcall
095600010202     C                   endif
095700010202     C*
095800010202     C                   ENDSR
095900141028     C*----------------------------------------------------*
096000141028     C*  CONTROLLO NUMERICITA' CAMPI
096100141028     C*----------------------------------------------------*
096200141028     C     CHKNUM        BEGSR
096300141028     C*
096400141028     C                   IF        PiDecChr = *blanks
096500141028     C                   EVAL      PiDecChr = CharNUM
096600141028     C                   ENDIF
096700141028     C*
096800141028     C                   callp     UBISNUM_Check(PiStr
096900141028     C                                          :PiDecChr
097000141028     C                                          :PiVal
097100141028     C                                          :PiNum
097200141028     C                                          :PiInt)
097300141028     C*
097400141028     C                   ENDSR
097500141028     C***
097600160809
097700160809
097800160809     C*----------------------------------------------------*
097900160809     C*  ROUTINE DI SPLITTING - TIPO 4
098000160809     C*----------------------------------------------------*
098100160809     C     SR_SPLIT4     BEGSR
098200160809     C*
098300160809     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
098400160809     C                   CALL      'XSPLIT2'
098500160809     C                   PARM                    InStringa
098600160809     C                   PARM                    InSepara
098700160809     C                   PARM      '4'           InTypeOut
098800160809     C                   PARM      ''            wSkParam
098900160809     C                   PARM                    OutErrore
099000160809     C                   PARM                    OutMsg
099100160809     C                   MOVEA     wSkParam      SkSplitCSV_4
099200160809     C*
099300160809     C                   ENDSR
099400141028
099500141028
099600141028     C*----------------------------------------------------*
099700160809     C*  ROUTINE DI SPLITTING - TIPO 5
099800141028     C*----------------------------------------------------*
099900160809     C     SR_SPLIT5     BEGSR
100000141028     C*
100100141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
100200141028     C                   CALL      'XSPLIT2'
100300141028     C                   PARM                    InStringa
100400141028     C                   PARM                    InSepara
100500141028     C                   PARM      '5'           InTypeOut
100600141028     C                   PARM      ''            wSkParam
100700141028     C                   PARM                    OutErrore
100800141028     C                   PARM                    OutMsg
100900141028     C                   MOVEA     wSkParam      SkSplitCSV_5
101000141028     C*
101100141028     C                   ENDSR
101200141028
101300141028
101400141028     C*----------------------------------------------------*
101500141028     C*  SCRITTURA BUFFER EDIVAT
101600141028     C*----------------------------------------------------*
101700141028     C     WRIVAT        BEGSR
101800141028     C*
101900141028     C                   IF        VATNOT <> *blanks
102000141028     C                   EVAL      VATFGS = VABFGS
102100141028     C                   EVAL      VATCCM = VABCCM
102200141028     C                   EVAL      VATLNP = VABLNP
102300141028     C                   EVAL      VATAAS = VABAAS
102400141028     C                   EVAL      VATNRS = VABNRS
102500141028     C                   EVAL      VATNSP = VABNSP
102600141028     C                   EVAL      VATCMR = VABCMR
102700141028     C                   EVAL      VATCNT = VABCNT
102800141028     C                   ADD       1             §CTROKVT
102900141028     C                   WRITE     EDIVAT00
103000141028     C                   ENDIF
103100141028     C*
103200141028     C                   ENDSR
103300000801
103400011113
103500011113     C*----------------------------------------------------*
103600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
103700011113     C*----------------------------------------------------*
103800011113     C     CHKIMPDIV     BEGSR
103900011113     C*
104000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
104100011113     C                   Z-ADD     *zeros        wrkDec            9 9
104200011113     C*
104300011113     C* Come prima cosa effettuo considerazioni sulla divisa
104400011113     C                   IF        vabIAS > *zeros
104500011113     C                   IF        vabVAS <> 'EUR'
104600011113     C                   EVAL      vabVAS =  'ITL'
104700011113     C                   ENDIF
104800011113     C                   ENDIF
104900011113     C*
105000011113     C                   IF        vabCAS > *zeros
105100011113     C                   IF        vabVCA <> 'EUR'
105200011113     C                   EVAL      vabVCA =  'ITL'
105300011113     C                   ENDIF
105400011113     C                   ENDIF
105500011113     C*
105600011113     C                   IF        vabVMD > *zeros
105700020305     C                   IF        vabVAD <> 'EUR'
105800011113     C                   EVAL      vabVAD =  'ITL'
105900011113     C                   ENDIF
106000011113     C                   ENDIF
106100011113     C*
106200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
106300011113     C                   Z-ADD     vabIAS        wrkDec
106400011113     C                   IF        wrkDec > *zeros
106500011113     C                   IF        vabVAS = 'ITL'
106600011113     C                   EVAL      vabIAS = *zeros
106700011113     C                   ENDIF
106800011113     C                   ENDIF
106900011113     C*
107000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
107100011113     C                   Z-ADD     vabCAS        wrkDec
107200011113     C                   IF        wrkDec > *zeros
107300011113     C                   IF        vabVCA = 'ITL'
107400011113     C                   EVAL      vabCAS = *zeros
107500011113     C                   ENDIF
107600011113     C                   ENDIF
107700011113     C*
107800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
107900011113     C                   Z-ADD     vabVMD        wrkDec
108000011113     C                   IF        wrkDec > *zeros
108100011113     C                   IF        vabVAD = 'ITL'
108200011113     C                   EVAL      vabVMD = *zeros
108300011113     C                   ENDIF
108400011113     C                   ENDIF
108500011113     C*
108600011113     C                   ENDSR
108700011113     C***
108800011113
108900011113
109000000801
109100000801
109200990920      /TITLE Invio dei dati al punto operativo.
109300010202     C     invio         BEGSR
109400990920     C*
109500140117     C* 1° invio EDIVAT
109600010201     C                   reset                   dscmz
109700010201     C                   move      vlrpoi        cmzdst
109800140117     C                   eval      cmzfld = 'EDIVATWR'
109900010201     C                   eval      cmzmbd = vlrhdl
110000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
110100021009     C***                if        prmfir = *blanks
110200140117     C                   eval      cmzfla = 'EDIVAT0F'
110300140117     C                   eval      cmzmba = 'EDIVAT0F'
110400021009     C***                else
110500021009     C***                eval      cmzfla = prmfir
110600021009     C***                eval      cmzmba = prmfir
110700021009     C***                endif
110800010201     C                   eval      cmznrr = *zeros
110900020305     C                   move      §ctrokvt      cmznrr
111000021018     C                   eval      cmzlba = vlrfl1
111100010201     C                   call(e)   'TIS711C'
111200010201     C                   parm                    dscmz
111300010201     C                   parm      *blanks       esito
111400010205     C                   if        %error
111500010205     C                             or cmzerr = '1'
111600010205     C                             or esito  = '1'
111700010205     C                   eval      wrkesito = '3'
111800010205     C                   else
111900010201     C*
112000140117     C* 2° invio EDIVAB
112100010201     C                   reset                   dscmz
112200010201     C                   move      vlrpoi        cmzdst
112300010201     C                   eval      cmzfld = vlrfou
112400010201     C                   eval      cmzmbd = vlrhdl
112500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
112600021009     C***                if        prmfir = *blanks
112700140117     C                   eval      cmzfla = 'EDIVAB0F'
112800140117     C                   eval      cmzmba = 'EDIVAB0F'
112900021009     C***                else
113000021009     C***                eval      cmzfla = prmfir
113100021009     C***                eval      cmzmba = prmfir
113200021009     C***                endif
113300010201     C                   eval      cmznrr = *zeros
113400010201     C                   move      §ctrokvb      cmznrr
113500021018     C                   eval      cmzlba = vlrfl1
113600010201     C                   call(e)   'TIS711C'
113700010201     C                   parm                    dscmz
113800010201     C                   parm      *blanks       esito
113900010201     C                   if        %error
114000010201     C                             or cmzerr = '1'
114100010201     C                             or esito  = '1'
114200010201     C                   eval      wrkesito = '3'
114300010201     C                   endif
114400010205     C                   endif
114500990920     C*
114600000613     C                   ENDSR
114700000613     C***
114800070411
114900141028
115000070411     C     *pssr         BEGSR
115100070411     C*
115200070411     C                   if        %open(tivin00r)
115300070411     C                   close     tivin00r
115400070411     C                   endif
115500140117     C                   if        %open(edivabwr)
115600140117     C                   close     edivabwr
115700070411     C                   endif
115800140117     C                   if        %open(edivatwr)
115900140117     C                   close     edivatwr
116000070411     C                   endif
116100070411     C*
116200070411     C* Effettuo la chiamata al CLLE preposto
116300140117     C                   call(e)   'TITVEVTC'
116400070411     C                   parm                    parccm
116500070411     C                   parm                    parmbr
116600070411     C                   parm      '2'           paropz
116700070411     C*
116800070411     C                   eval      wrkesito = '2'
116900070411     C*
117000070411     C                   seton                                        LR
117100070411     C*
117200070411     C                   ENDSR     '*CANCL'
117300070411     C***
117400070411
117500990910
117600000613     C     *inzsr        BEGSR
117700990910     C*
117800990910     C     *entry        plist
117900990920     C                   parm                    tivlrds
118000990921     C                   parm      wrkesito      esito
118100000724     C                   parm                    prmlit
118200000710     C                   parm                    prmfir
118300000613     C*
118400000830     C* CALCOLA LA DATA CORRENTE
118500140117     C                   time                    wn14             14 0
118600140117     C                   movel     wn14          oracor            6 0          *ORA
118700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
118800100722     C                   eval      datcor = %dec(%date() : *ISO)
118900000830     C*
119000000613     C                   ENDSR
119100000613     C***
