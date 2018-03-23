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
005700141028     D SkSplitCSV_5    S            256    DIM(256)
005800141028
005900000830
006000041025     D*------------------
006100041025     D* DS REPERIMENTO NUMERATORE
006200041025     D*------------------
006300041025     D trul33ds      e ds                  inz
006400041025     D*------------------
006500041025     D* DS ARCHITETTURA
006600041025     D*------------------
006700041025     D kpjba         e ds                  inz
006800041025     D*------------------
006900990908
007000141028
007100141028     D*------------------
007200141028     D* LINKING A DEFINIZIONI ESTERNE
007300141028     D*------------------
007400141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007500141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007600141028
007700141028
007800010201
007900010201
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008500070719     C*
008600070719     C* Esegue lancio TISI95R solo x chiusura
008700070719     C                   CLEAR                   TISI95DS
008800070719     C                   EVAL      I95TLA = 'C'
008900070719     C                   CALL      'TISI95R'
009000070719     C                   PARM                    TISI95DS
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300140922     C                   call(e)   'TITVEVTC'
009400140922     C                   parm                    parccm
009500140922     C                   parm                    parmbr
009600140922     C                   parm      '2'           paropz
009700170731     C*
009800170731     C                   if        *in53
009900170731     C                   call      'TIS7P2SER'
010000170731     C                   parm      'C'           pIn_Opz           1
010100170731     C                   parm                    tivlrds
010200170731     C                   parm                    EDIVABDS
010300170731     C                   parm      *blanks       pIn_CdIdAz        2
010400170731     C                   parm      *blanks       pIn_MaskPDF      50
010500170731     C                   parm      *blanks       pOut_Esito        1
010600170731     C                   endif
010700000616     C*
010800010201     C                   seton                                        LR
010900990908
011000000801
011100910830     C*--------------------------------------------------------
011200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011300910830     C*--------------------------------------------------------
011400040526     C     RWFILE        BEGSR
011500990910     C*
011600990914     C                   if        not %open(tivin00r)
011700990908     C                   open      tivin00r
011800990914     C                   endif
011900140117     C                   if        not %open(edivabwr)
012000140117     C                   open      edivabwr
012100990914     C                   endif
012200141028     C*
012300140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012400020305     C                   exsr      prevat
012500010201     C*
012600010202     C                   if        chkcall = '0'
012700010202     C*
012800140117     C                   if        not %open(edivatwr)
012900140117     C                   open      edivatwr
013000010201     C                   endif
013100990910     C*
013200010201     C                   clear                   §CTROKVB          5 0
013300020305     C                   clear                   §CTROKVT          5 0
013400000801     C                   clear                   §CTRMO            5 0
013500000801     C                   clear                   §CTRNO            5 0
013600141028     C*
013700141028     C* Inizializzazioni fuori ciclo
013800150127     C                   SETOFF                                       40
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
015000150127     C*
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
023500141028     C*
023600150127     C                   if        VABNCL = *zeros
023700150127     C                   eval      VABNCL = cntVABNCL
023800150127     C                   endif
023900150127     C*
024000141028     C                   if        VABPRD = *blanks
024100141028     C* ......VABPRD
024200141028     C* Reperisco la provincia dal CAP e dalla località
024300141028     C                   IF        VABLOD <> *blanks AND
024400141028     C                             VABCAD <> *blanks AND
024500141028     C                             VABNZD  = *blanks
024600141028     C                   CLEAR                   TISI95DS
024700141028     C                   EVAL      I95TCN = '3'
024800141028     C                   Z-ADD     datcor        I95DAT
024900141028     C                   EVAL      I95CAP = VABCAD
025000141028     C                   EVAL      I95LOC = VABLOD
025100141028     C                   CALL      'TISI95R'
025200141028     C                   PARM                    TISI95DS
025300141028     C                   EVAL      VABPRD = O95PRV
025400141028     C                   ENDIF
025500141028     C                   endif
025600140922     C*
025700140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
025800141028     C                   IF        wFlgCAS <> '0'
025900140922     C                   IF        VABCBO = '1'
026000140922     C                   EVAL      VABCBO = '4'
026100140922     C                   ELSE
026200140922     C                   EVAL      VABCBO = '6'
026300140922     C                   ENDIF
026400140922     C                   ENDIF
026500140922     C*
026600140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
026700141028     C                   EXSR      CHKIMPDIV
026800140922     C*
026900140922     C                   ENDSR
027000141028     C*----------------------------------------------------*
027100141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027200141028     C*----------------------------------------------------*
027300141028     C     INZVAR        BEGSR
027400141028     C*
027500141028     C* Inizializzo variabili di wrk
027600141028     C                   Z-ADD     *zeros        Num5_0            5 0
027700150127     C                   Z-ADD     *zeros        cntVABNCL         5 0
027800141028     C                   MOVEL     '0'           wFlgCAS
027900141028     C                   MOVEL     *blanks       wVATNOT_A
028000141028     C                   MOVEL     *blanks       wVATNOT_B
028100141028     C                   MOVEL     *blanks       wVATNOT_E
028200141028     C                   MOVEL     *blanks       wVATNOT_I
028300141028     C                   MOVEL     *blanks       wVATNOT_J
028400141028     C                   MOVEL     *blanks       wVATNOT_S
028500141028     C                   MOVEL     *blanks       wVATNOT_P
028600141028     C                   CLEAR                   SkSplitCSV_5
028700141028     C*
028800141028     C* Reimposto i valori di default
028900141028     C                   EXSR      DEFCAM
029000141028     C*
029100141028     C                   ENDSR
029200141028     C*----------------------------------------------------*
029300141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029400141028     C*----------------------------------------------------*
029500141028     C     DEFCAM        BEGSR
029600141028     C*
029700141028     C                   CLEAR                   EDIVAB00
029800141028     C                   CLEAR                   EDIVAT00
029900141028     C*
030000141028     C* Imposto i valori bolla di default
030100141028     C                   EVAL      VABCTR = 000
030200141028     C                   EVAL      VABCBO = '1'
030300150127     C                   EVAL      VABTSP = 'C'
030400141028     C*
030500141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030600141028     C* e delimitatore testo.
030700141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030800141028     C*
030900141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031000141028     C                   EVAL      CharSOS = CharNUM
031100141028     C*
031200141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
031300141028     C                   SETOFF                                       53
031400141028     C                   SELECT
031500141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
031600141028     C                   SETON                                        53
031700141028     C                   ENDSL
031800141028     C*
031900141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032000141028     C                   EVAL      posDaDft = 1
032100141028     C                   EVAL      posADft  = 0
032200141028     C                   EVAL      wGiroDft = 0
032300141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032400141028     C                             posDaDft > 0
032500141028     C*
032600141028     C* Gestisco il 1° giro
032700141028     C                   IF        wGiroDft = 0
032800141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
032900141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033000141028     C* Incremento il contatore dei "giri"
033100141028     C                   EVAL      wGiroDft = 1
033200141028     C                   ELSE
033300141028     C                   EVAL      posDaDft = posADft
033400141028     C                   ENDIF
033500141028     C* Eseguo lo scan x trovare la fine del campo corrente
033600141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033700141028     C*
033800141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
033900141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
034000141028     C                   IF        posDaDft > 0 AND
034100141028     C                             posADft  > 0
034200141028     C* NCL
034300141028     C                   IF        %subst(
034400141028     C                             %subst(vlrppt:posDaDft+1:
034500141028     C                             posADft-posDaDft-1):1:3)
034600141028     C                             = 'NCL'
034700141028     C                   EVAL      PiStr=%trim(%subst(
034800141028     C                             %subst(vlrppt:posDaDft+1:
034900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035000141028     C                   EXSR      CHKNUM
035100141028     C                   IF        PiInt=*on
035200141028     C                   Z-ADD     PiVal         VABNCL
035300141028     C                   ENDIF
035400141028     C                   ENDIF
035500141028     C* CCM
035600141028     C                   IF        %subst(
035700141028     C                             %subst(vlrppt:posDaDft+1:
035800141028     C                             posADft-posDaDft-1):1:3)
035900141028     C                             = 'CCM'
036000141028     C                   EVAL      PiStr=%trim(%subst(
036100141028     C                             %subst(vlrppt:posDaDft+1:
036200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036300141028     C                   EXSR      CHKNUM
036400141028     C                   IF        PiInt=*on
036500141028     C                   Z-ADD     PiVal         VABCCM
036600141028     C                   ENDIF
036700141028     C                   ENDIF
036800141028     C* LNP
036900141028     C                   IF        %subst(
037000141028     C                             %subst(vlrppt:posDaDft+1:
037100141028     C                             posADft-posDaDft-1):1:3)
037200141028     C                             = 'LNP'
037300141028     C                   EVAL      PiStr=%trim(%subst(
037400141028     C                             %subst(vlrppt:posDaDft+1:
037500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037600141028     C                   EXSR      CHKNUM
037700141028     C                   IF        PiInt=*on
037800141028     C                   Z-ADD     PiVal         VABLNP
037900141028     C                   ENDIF
038000141028     C                   ENDIF
038100141028     C* NRS
038200141028     C                   IF        %subst(
038300141028     C                             %subst(vlrppt:posDaDft+1:
038400141028     C                             posADft-posDaDft-1):1:3)
038500141028     C                             = 'NRS'
038600141028     C                   EVAL      PiStr=%trim(%subst(
038700141028     C                             %subst(vlrppt:posDaDft+1:
038800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038900141028     C                   EXSR      CHKNUM
039000141028     C                   IF        PiInt=*on
039100141028     C                   Z-ADD     PiVal         VABNRS
039200141028     C                   ENDIF
039300141028     C                   ENDIF
039400141028     C* CTR
039500141028     C                   IF        %subst(
039600141028     C                             %subst(vlrppt:posDaDft+1:
039700141028     C                             posADft-posDaDft-1):1:3)
039800141028     C                             = 'CTR'
039900141028     C                   EVAL      PiStr=%trim(%subst(
040000141028     C                             %subst(vlrppt:posDaDft+1:
040100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040200141028     C                   EXSR      CHKNUM
040300141028     C                   IF        PiInt=*on
040400141028     C                   Z-ADD     PiVal         VABCTR
040500141028     C                   ENDIF
040600141028     C                   ENDIF
040700141028     C* PKB
040800141028     C                   IF        %subst(
040900141028     C                             %subst(vlrppt:posDaDft+1:
041000141028     C                             posADft-posDaDft-1):1:3)
041100141028     C                             = 'PKB'
041200141028     C                   EVAL      PiStr=%trim(%subst(
041300141028     C                             %subst(vlrppt:posDaDft+1:
041400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500141028     C                   EXSR      CHKNUM
041600141028     C                   IF        PiNum=*on
041700141028     C                   Z-ADD     PiVal         VABPKB
041800141028     C                   ENDIF
041900141028     C                   ENDIF
042000141028     C* VLB
042100141028     C                   IF        %subst(
042200141028     C                             %subst(vlrppt:posDaDft+1:
042300141028     C                             posADft-posDaDft-1):1:3)
042400141028     C                             = 'VLB'
042500141028     C                   EVAL      PiStr=%trim(%subst(
042600141028     C                             %subst(vlrppt:posDaDft+1:
042700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800141028     C                   EXSR      CHKNUM
042900141028     C                   IF        PiNum=*on
043000141028     C                   Z-ADD     PiVal         VABVLB
043100141028     C                   ENDIF
043200141028     C                   ENDIF
043300141028     C* QFT
043400141028     C                   IF        %subst(
043500141028     C                             %subst(vlrppt:posDaDft+1:
043600141028     C                             posADft-posDaDft-1):1:3)
043700141028     C                             = 'QFT'
043800141028     C                   EVAL      PiStr=%trim(%subst(
043900141028     C                             %subst(vlrppt:posDaDft+1:
044000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044100141028     C                   EXSR      CHKNUM
044200141028     C                   IF        PiNum=*on
044300141028     C                   Z-ADD     PiVal         VABQFT
044400141028     C                   ENDIF
044500141028     C                   ENDIF
044600141028     C* CBO
044700141028     C                   IF        %subst(
044800141028     C                             %subst(vlrppt:posDaDft+1:
044900141028     C                             posADft-posDaDft-1):1:3)
045000141028     C                             = 'CBO'
045100141028     C                   EVAL      VABCBO=%trim(%subst(
045200141028     C                             %subst(vlrppt:posDaDft+1:
045300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400141028     C                   ENDIF
045500141028     C* TSP
045600141028     C                   IF        %subst(
045700141028     C                             %subst(vlrppt:posDaDft+1:
045800141028     C                             posADft-posDaDft-1):1:3)
045900141028     C                             = 'TSP'
046000141028     C                   EVAL      VABTSP=%trim(%subst(
046100141028     C                             %subst(vlrppt:posDaDft+1:
046200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300141028     C                   ENDIF
046400141028     C* VAS
046500141028     C                   IF        %subst(
046600141028     C                             %subst(vlrppt:posDaDft+1:
046700141028     C                             posADft-posDaDft-1):1:3)
046800141028     C                             = 'VAS'
046900141028     C                   EVAL      VABVAS=%trim(%subst(
047000141028     C                             %subst(vlrppt:posDaDft+1:
047100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200141028     C                   ENDIF
047300141028     C* VCA
047400141028     C                   IF        %subst(
047500141028     C                             %subst(vlrppt:posDaDft+1:
047600141028     C                             posADft-posDaDft-1):1:3)
047700141028     C                             = 'VCA'
047800141028     C                   EVAL      VABVCA=%trim(%subst(
047900141028     C                             %subst(vlrppt:posDaDft+1:
048000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100141028     C                   ENDIF
048200141028     C* TIC
048300141028     C                   IF        %subst(
048400141028     C                             %subst(vlrppt:posDaDft+1:
048500141028     C                             posADft-posDaDft-1):1:3)
048600141028     C                             = 'TIC'
048700141028     C                   EVAL      VABTIC=%trim(%subst(
048800141028     C                             %subst(vlrppt:posDaDft+1:
048900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000141028     C                   ENDIF
049100141028     C* GCA
049200141028     C                   IF        %subst(
049300141028     C                             %subst(vlrppt:posDaDft+1:
049400141028     C                             posADft-posDaDft-1):1:3)
049500141028     C                             = 'GCA'
049600141028     C                   EVAL      VABGCA=%trim(%subst(
049700141028     C                             %subst(vlrppt:posDaDft+1:
049800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900141028     C                   ENDIF
050000141028     C* CTM
050100141028     C                   IF        %subst(
050200141028     C                             %subst(vlrppt:posDaDft+1:
050300141028     C                             posADft-posDaDft-1):1:3)
050400141028     C                             = 'CTM'
050500141028     C                   EVAL      VABCTM=%trim(%subst(
050600141028     C                             %subst(vlrppt:posDaDft+1:
050700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800141028     C                   ENDIF
050900141028     C* FFD
051000141028     C                   IF        %subst(
051100141028     C                             %subst(vlrppt:posDaDft+1:
051200141028     C                             posADft-posDaDft-1):1:3)
051300141028     C                             = 'FFD'
051400141028     C                   EVAL      VABFFD=%trim(%subst(
051500141028     C                             %subst(vlrppt:posDaDft+1:
051600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700141028     C                   ENDIF
051800141028     C* VAD
051900141028     C                   IF        %subst(
052000141028     C                             %subst(vlrppt:posDaDft+1:
052100141028     C                             posADft-posDaDft-1):1:3)
052200141028     C                             = 'VAD'
052300141028     C                   EVAL      VABVAD=%trim(%subst(
052400141028     C                             %subst(vlrppt:posDaDft+1:
052500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600141028     C                   ENDIF
052700141028     C* GMA
052800141028     C                   IF        %subst(
052900141028     C                             %subst(vlrppt:posDaDft+1:
053000141028     C                             posADft-posDaDft-1):1:3)
053100141028     C                             = 'GMA'
053200141028     C                   EVAL      VABGMA=%trim(%subst(
053300141028     C                             %subst(vlrppt:posDaDft+1:
053400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500141028     C                   ENDIF
053600141028     C* GGA
053700141028     C                   IF        %subst(
053800141028     C                             %subst(vlrppt:posDaDft+1:
053900141028     C                             posADft-posDaDft-1):1:3)
054000141028     C                             = 'GGA'
054100141028     C                   EVAL      VABGGA=%trim(%subst(
054200141028     C                             %subst(vlrppt:posDaDft+1:
054300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400141028     C                   ENDIF
054500141028     C* GVA
054600141028     C                   IF        %subst(
054700141028     C                             %subst(vlrppt:posDaDft+1:
054800141028     C                             posADft-posDaDft-1):1:3)
054900141028     C                             = 'GVA'
055000141028     C                   EVAL      VABGVA=%trim(%subst(
055100141028     C                             %subst(vlrppt:posDaDft+1:
055200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300141028     C                   ENDIF
055400141028     C* TC1
055500141028     C                   IF        %subst(
055600141028     C                             %subst(vlrppt:posDaDft+1:
055700141028     C                             posADft-posDaDft-1):1:3)
055800141028     C                             = 'TC1'
055900141028     C                   EVAL      VABTC1=%trim(%subst(
056000141028     C                             %subst(vlrppt:posDaDft+1:
056100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200141028     C                   ENDIF
056300141028     C* TC2
056400141028     C                   IF        %subst(
056500141028     C                             %subst(vlrppt:posDaDft+1:
056600141028     C                             posADft-posDaDft-1):1:3)
056700141028     C                             = 'TC2'
056800141028     C                   EVAL      VABTC2=%trim(%subst(
056900141028     C                             %subst(vlrppt:posDaDft+1:
057000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100141028     C                   ENDIF
057200141028     C* VATTRC
057300141028     C                   IF        %subst(
057400141028     C                             %subst(vlrppt:posDaDft+1:
057500141028     C                             posADft-posDaDft-1):1:3)
057600141028     C                             = 'TRC'
057700141028     C                   EVAL      VATTRC=%trim(%subst(
057800141028     C                             %subst(vlrppt:posDaDft+1:
057900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000141028     C                   ENDIF
058100141028     C* ...
058200141028     C                   ENDIF
058300141028     C                   ENDDO
058400141028     C*
058500141028     C                   ENDSR
058600000801     C*----------------------------------------------------*
058700140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
058800000801     C*----------------------------------------------------*
058900040910     C     IMPVABVAT     BEGSR
059000040910     C*
059100040910     C* Traduzione relativa ai tipi record del file del cliente
059200040910     C*
059300071210     C*
059400071210     C***
059500141028     C* ...tipo record 'UNB' (info CMR)
059600141028     C                   EVAL      wTag = 'UNB'
059700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
059800141028     C                   MOVEL     *blanks       savCMR           35
059900141028     C                   EVAL      InStringa =
060000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
060100141028     C                   EVAL      InSepara = '+'
060200141028     C                   EXSR      SR_SPLIT
060300141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
060400140922     C                                      %char(datcor) + ' ' + %char(oracor)
060500130620     C                   ENDIF
060600150127     C*
060700150127     C***
060800150127     C* ...tipo record 'NAD+SF' (info testata Mittente Originale)
060900150127     C                   EVAL      wTag = 'NAD+SF'
061000150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061100150127     C                   MOVEL     *blanks       savRMO           25
061200150127     C                   MOVEL     *blanks       savCMO            9
061300150127     C                   MOVEL     *blanks       savNMO            3
061400150127     C                   EVAL      InStringa =
061500150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
061600150127     C                   EVAL      InSepara = '+'
061700150127     C                   EXSR      SR_SPLIT
061800150127     C                   EVAL      savRMO = %trim(SkSplitCSV_5(3))
061900150127     C                   EVAL      savCMO = %trim(SkSplitCSV_5(7))
062000150127     C                   EVAL      savNMO = %trim(SkSplitCSV_5(8))
062100150127     C                   IF        savNMO = 'I'   OR
062200150127     C                             savNMO = 'IT'  OR
062300150127     C                             savNMO = 'ITA'
062400150127     C                   EVAL      savNMO = *blanks
062500150127     C                   ENDIF
062600150127     C                   ENDIF
062700150127     C*
062800150127     C***
062900150127     C* ...tipo record 'BGM+87' (info testata)
063000150127     C                   EVAL      wTag = 'BGM+87'
063100150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
063200150127     C                   MOVEL     *blanks       savNAS           15
063300150127     C                   EVAL      InStringa =
063400150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
063500150127     C                   EVAL      InSepara = '+'
063600150127     C                   EXSR      SR_SPLIT
063700150127     C                   EVAL      savNAS = %trim(SkSplitCSV_5(1))
063800150127     C                   ENDIF
063900141028     C*
064000141028     C***
064100150127     C* ...tipo record 'GID' (rottuta di codice spedizione)
064200150127     C                   EVAL      wTag = 'GID'
064300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064400150127     C*
064500150127     C* Scarico testata in sospeso
064600150127     C                   IF        *in40
064700150127     C  N31              ADD       1             §CTROKVB
064800150127     C  N31              exsr      CHKWRI
064900150127     C  N31              WRITE     EDIVAB00
065000150127     C                   ELSE
065100150127     C                   SETON                                        40
065200150127     C                   ENDIF
065300150127     C*
065400150127     C* Inizio nuova bolla
065500141028     C                   SETOFF                                       31
065600141028     C                   eval      vinflg = '1'
065700141028     C* ......inizializzazioni iniziali e formati record file Bartolini
065800141028     C                   EXSR      INZVAR
065900141028     C* ......valorizzazione campi fissi
066000141028     C                   MOVEL     datcor        VABAAS
066100141028     C                   MOVE      datcor        VABMGS
066200141028     C                   MOVE(P)   vlrpoi        VABFGS
066300141028     C                   EVAL      VABCMR = savCMR
066400141028     C                   EVAL      VABDCM = datcor
066500141028     C                   EVAL      VABDTS = datcor
066600141028     C                   EVAL      VABCNT = 1
066700150127     C                   EVAL      VABRMO = savRMO
066800150127     C                   EVAL      VABCMO = savCMO
066900150127     C                   EVAL      VABNMO = savNMO
067000150127 xxx C                   EVAL      VABNAS = savNAS
067100141028     C* ......NSP => Stacco un numeratore da AZNUM
067200141028     C                   clear                   TRUL33DS
067300141028     C                   eval      I33OPE = *zeros
067400141028     C                   eval      I33CNU = 302
067500141028     C                   eval      I33NUM = 1
067600141028     C                   movel     TRUL33DS      KPJBU
067700141028     C                   call      'TRUL33R'
067800141028     C                   parm                    KPJBA
067900141028     C                   movel     KPJBU         TRUL33DS
068000141028     C                   if        O33ERR = *zeros
068100141028     C                   z-add     O33NRF        VABNSP
068200141028     C                   else
068300141028     C                   Z-ADD     1             wRecErr
068400141028     C                   SETON                                        31
068500141028     C                   EVAL      vinmsg = %trimr(vinmsg)
068600141028     C                             + ' ' + 'VABNSP VATNSP'
068700141028     C                   endif
068800141028     C                   ENDIF
068900150127     C*
069000150127     C***
069100150127     C* ...tipo record 'NAD+CN' (destinatario)
069200150127     C                   EVAL      wTag = 'NAD+CN'
069300150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069400150127     C                   EVAL      InStringa =
069500150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
069600150127     C                   EVAL      InSepara = '+'
069700150127     C                   EXSR      SR_SPLIT
069800150921     C                   MOVEL     *blanks       wSgm2          1024
069900150921     C                   MOVEL     *blanks       wSgm4          1024
070000150921     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(2))
070100150921     C                   EVAL      wSgm4 = %trim(SkSplitCSV_5(4))
070200150127     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
070300150127     C***                EVAL      VABPRD = %trim(SkSplitCSV_5(6))
070400150127     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
070500150127     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
070600150127     C                   IF        VABNZD = 'I'   OR
070700150127     C                             VABNZD = 'IT'  OR
070800150127     C                             VABNZD = 'ITA'
070900150127     C                   EVAL      VABNZD = *blanks
071000150127     C                   ENDIF
071100150902     C*
071200150921     C                   MOVEL     *blanks       wRSD             70
071300150921     C                   EVAL      InStringa = wSgm2
071400150902     C                   EVAL      InSepara = ':'
071500150902     C                   EXSR      SR_SPLIT
071600150921     C
071700150921     C                   EVAL      wRSD = %trim(SkSplitCSV_5(1))
071800150921     C                   EVAL      VABRSD = %subst(wRSD:1:35)
071900150921     C                   EVAL      VABRD2 = %subst(wRSD:1+35:35)
072000150902     C*
072100150921     C                   EVAL      InStringa = wSgm4
072200150127     C                   EVAL      InSepara = ':'
072300150127     C                   EXSR      SR_SPLIT
072400150127     C                   EVAL      VABIND = %trim(SkSplitCSV_5(1))
072500150127     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
072600150127     C                                      %trim(SkSplitCSV_5(4))+
072700150127     C                                      %trim(SkSplitCSV_5(5))
072800150127     C                   ENDIF
072900141028     C*
073000141028     C***
073100150127     C* ...tipo record 'RFF+IV' (riferimento spedizione alfabetico)
073200150127     C                   EVAL      wTag = 'RFF+IV'
073300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073400141028     C                   EVAL      VABRMA =
073500141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
073600141028     C                   ENDIF
073700150127     C*
073800150127     C***
073900150127     C* ...tipo record 'RFF+TB' (riferimento spedizione numerico)
074000150127     C                   EVAL      wTag = 'RFF+TB'
074100150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074200150127     C                   EVAL      InStringa =
074300150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
074400150127     C                   EVAL      PiStr=InStringa
074500150127     C                   EXSR      CHKNUM
074600150127     C                   IF        PiNum=*on
074700150127     C                   Z-ADD     PiVal         VABRMN
074800150127     C                   ELSE
074900150127     C                   Z-ADD     1             wRecErr
075000150127     C                   ENDIF
075100150127     C                   ENDIF
075200150127     C*
075300150127     C***
075400150127     C* ...tipo record 'RFF+CR' ("chi sono")
075500150127     C                   EVAL      wTag = 'RFF+CR'
075600150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075700150127     C                   EVAL      VATNOT =
075800150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
075900150127     C                   EVAL      VATTRC = 'E'
076000150127     C                   EXSR      WRIVAT
076100150127     C                   ADD       1             cntVABNCL
076200150127     C                   ENDIF
076300150127     C*
076400150127     C***
076500150127     C* ...tipo record 'RFF+TE' (telefono referente consegna + nome referente consegna)
076600150127     C                   EVAL      wTag = 'RFF+TE'
076700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076800150127     C                   EVAL      VATNOT =
076900150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
077000150127     C                   EVAL      VATTRC = 'B'
077100150127     C                   EXSR      WRIVAT
077200150127     C*
077300150127     C                   EVAL      VATNOT = VABRSD
077400150127     C                   EVAL      VATTRC = 'A'
077500150127     C                   EXSR      WRIVAT
077600150127     C                   ENDIF
077700150127     C*
077800150127     C***
077900150127     C* ...tipo record 'TSR' (servizi speciali)
078000150127     C                   EVAL      wTag = 'TSR'
078100150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078200150127     C                   EVAL      InStringa =
078300150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
078400150127     C                   EVAL      InSepara = '+'
078500150127     C                   EXSR      SR_SPLIT
078600150127     C                   SELECT
078700150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
078800150127     C                             %trim(SkSplitCSV_5(3)) = 'PRN'
078900150127     C                   EVAL      VABTSP = 'E'
079000150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
079100150127     C                             %trim(SkSplitCSV_5(3)) = '10H'
079200150127     C                   EVAL      VABTSP = 'H'
079300150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
079400150127     C                             %trim(SkSplitCSV_5(3)) = 'GWC'
079500150127     C                   EVAL      VABFFD = 'S'
079600150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
079700150127     C                             %trim(SkSplitCSV_5(3)) = 'BKI'
079800150127     C                   SELECT
079900150127     C                   WHEN      VABTC1 = *blanks
080000150127     C                   EVAL      VABTC1 = 'A'
080100150127     C                   WHEN      VABTC2 = *blanks
080200150127     C                   EVAL      VABTC2 = 'A'
080300150127     C                   ENDSL
080400150127     C                   ENDSL
080500150127     C                   ENDIF
080600150127     C*
080700150127     C***
080800150127     C* ...tipo record 'DTM+Z73' (servizi speciali)
080900150127     C                   EVAL      wTag = 'DTM+Z73'
081000150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081100150127     C                   EVAL      InStringa =
081200150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
081300150127     C                   EVAL      InSepara = ':'
081400150127     C                   EXSR      SR_SPLIT
081500150127     C                   SELECT
081600150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '102'
081700150127     C                   EVAL      VABTCR = *blanks
081800150127     C                   EVAL      InStringa =
081900150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
082000150127     C                   EVAL      PiStr=InStringa
082100150127     C                   EXSR      CHKNUM
082200150127     C                   IF        PiNum=*on
082300150127     C                   Z-ADD     PiVal         VABDCR
082400150127     C                   ELSE
082500150127     C                   Z-ADD     1             wRecErr
082600150127     C                   ENDIF
082700150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '203'
082800150127     C                   EVAL      VABTCR = *blanks
082900150127     C                   EVAL      InStringa =
083000150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
083100150127     C                   EVAL      PiStr=InStringa
083200150127     C                   EXSR      CHKNUM
083300150127     C                   IF        PiNum=*on
083400150127     C                   Z-ADD     PiVal         VABDCR
083500150127     C                   ELSE
083600150127     C                   Z-ADD     1             wRecErr
083700150127     C                   ENDIF
083800150127     C                   EVAL      InStringa =
083900150127     C                                %subst(%trim(SkSplitCSV_5(1)):9:4)
084000150127     C                   EVAL      PiStr=InStringa
084100150127     C                   EXSR      CHKNUM
084200150127     C                   IF        PiNum=*on
084300150127     C                   Z-ADD     PiVal         VABHCR
084400150127     C                   ELSE
084500150127     C                   Z-ADD     1             wRecErr
084600150127     C                   ENDIF
084700150127     C                   ENDSL
084800150127     C                   ENDIF
084900141028     C*
085000141028     C***
085100141028     C* ...tipo record 'FTX+AAA' (natura merce)
085200150127     C***                EVAL      wTag = 'FTX+AAA'
085300150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085400150127     C***                EVAL      InStringa =
085500150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
085600150127     C***                EVAL      InSepara = '+'
085700150127     C***                EXSR      SR_SPLIT
085800150127     C***                EVAL      VABNAS = %trim(SkSplitCSV_5(3))
085900150127     C***                ENDIF
086000141028     C*
086100141028     C***
086200150127     C* ...tipo record 'MEA+WX+B' (peso spedizione)
086300160126     C***                EVAL      wTag = 'MEA+WX+B'
086400160126     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086500160126     C***                EVAL      InStringa =
086600160126     C***                             %subst(wvindta:%len(%trim(wTag))+2)
086700160126     C***                EVAL      InSepara = ':'
086800160126     C***                EXSR      SR_SPLIT
086900160126     C***                EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
087000160126     C***                EXSR      CHKNUM
087100160126     C***                IF        PiNum=*on
087200160126     C***                ADD       PiVal         VABPKB
087300160128     C***                IF        VABPKB = *zeros
087400160128     C***                EVAL      VABPKB = 0,1
087500160128     C***                ENDIF
087600160126     C***                ELSE
087700160126     C***                Z-ADD     1             wRecErr
087800160126     C***                ENDIF
087900160126     C***                ENDIF
088000160126     C*
088100160126     C***
088200160127     C* ...tipo record 'MEA+WT+G' (peso spedizione)
088300160127     C                   EVAL      wTag = 'MEA+WT+G'
088400160126     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088500160126     C                   EVAL      InStringa =
088600160126     C                                %subst(wvindta:%len(%trim(wTag))+2)
088700160126     C                   EVAL      InSepara = ':'
088800160126     C                   EXSR      SR_SPLIT
088900160126     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
089000160126     C                   EXSR      CHKNUM
089100160126     C                   IF        PiNum=*on
089200160126     C                   ADD       PiVal         VABPKB
089300160128     C                   IF        VABPKB = *zeros
089400160128     C                   EVAL      VABPKB = 0,1
089500160128     C                   ENDIF
089600160126     C                   ELSE
089700160126     C                   Z-ADD     1             wRecErr
089800160126     C                   ENDIF
089900160126     C                   ENDIF
090000141028     C*
090100141028     C***
090200141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
090300150127     C***                EVAL      wTag = 'MEA+VOL'
090400150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090500150127     C***                EVAL      InStringa =
090600150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
090700150127     C***                EVAL      InSepara = ':'
090800150127     C***                EXSR      SR_SPLIT
090900150127     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
091000150127     C***                EXSR      CHKNUM
091100150127     C***                IF        PiNum=*on
091200150924     C***                ADD       PiVal         VABVLB
091300150127     C***                ELSE
091400150127     C***                Z-ADD     1             wRecErr
091500150127     C***                ENDIF
091600150127     C***                ENDIF
091700141028     C*
091800141028     C***
091900141028     C* ...tipo record '???' (PDF per packing-list)
092000141028     C                   EVAL      wTag = '???PDF'
092100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092200141028     C*
092300141028     C* ......VATNOT_P
092400141028     C                   if        *in53
092500141028     C*
092600141028     C* Solo se valorizzato RMN
092700141028     C                   if        VABRMA <> *blanks
092800141028     C                   eval      pIn_MaskPDF =
092900141028     C                                     %trim(%subst(wvindta:1292:100))
093000141028     C                   call      'TIS7P2SER'
093100141028     C                   parm      'W'           pIn_Opz           1
093200141028     C                   parm                    tivlrds
093300141028     C                   parm                    EDIVABDS
093400141028     C                   parm      '10'          pIn_CdIdAz        2
093500141028     C                   parm                    pIn_MaskPDF      50
093600141028     C                   parm      *blanks       pOut_Esito        1
093700141028     C                   endif
093800141028     C                   endif
093900141028     C*
094000141028     C                   ENDIF
094100141028     C*
094200141028     C***
094300150127     C* ...tipo record 'UNT' (fine bolla)
094400141028     C                   EVAL      wTag = 'UNT'
094500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
094600141028     C  N31              ADD       1             §CTROKVB
094700141028     C  N31              exsr      CHKWRI
094800140117     C  N31              WRITE     EDIVAB00
094900100722     C                   endif
095000010202     C*
095100000801     C* Ebbene...
095200000801     C                   ADD       1             §CTRMO
095300141028     C                   IF        wRecErr <> *zeros
095400000801     C                   ADD       1             §CTRNO
095500000801     C                   EVAL      vinflg = '2'
095600000801     C                   ENDIF
095700000801     C*
095800000801     C                   ENDSR
095900010202     C*----------------------------------------------------*
096000140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
096100010202     C*----------------------------------------------------*
096200020305     C     PREVAT        BEGSR
096300010202     C*
096400140117     C* Compongo il nome del membro da dare al EDIVATWR
096500010202     C                   eval      parmbr = vlrhdl
096600010202     C                   movel     'M'           parmbr
096700060113     C                   eval      parccm = vlrksc
096800010202     C                   eval      paropz = '1'
096900010202     C* Effettuo la chiamata al CLLE preposto
097000140117     C                   call(e)   'TITVEVTC'
097100010202     C                   parm                    parccm
097200010202     C                   parm                    parmbr
097300010202     C                   parm                    paropz
097400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
097500010202     C                   if        %error
097600010202     C                   movel     '1'           chkcall
097700010202     C                   else
097800010202     C                   movel     '0'           chkcall
097900010202     C                   endif
098000010202     C*
098100010202     C                   ENDSR
098200141028     C*----------------------------------------------------*
098300141028     C*  CONTROLLO NUMERICITA' CAMPI
098400141028     C*----------------------------------------------------*
098500141028     C     CHKNUM        BEGSR
098600141028     C*
098700141028     C                   IF        PiDecChr = *blanks
098800141028     C                   EVAL      PiDecChr = CharNUM
098900141028     C                   ENDIF
099000141028     C*
099100141028     C                   callp     UBISNUM_Check(PiStr
099200141028     C                                          :PiDecChr
099300141028     C                                          :PiVal
099400141028     C                                          :PiNum
099500141028     C                                          :PiInt)
099600141028     C*
099700141028     C                   ENDSR
099800141028     C***
099900141028
100000141028
100100141028     C*----------------------------------------------------*
100200141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
100300141028     C*----------------------------------------------------*
100400141028     C     SR_SPLIT      BEGSR
100500141028     C*
100600141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
100700141028     C                   CALL      'XSPLIT2'
100800141028     C                   PARM                    InStringa
100900141028     C                   PARM                    InSepara
101000141028     C                   PARM      '5'           InTypeOut
101100141028     C                   PARM      ''            wSkParam
101200141028     C                   PARM                    OutErrore
101300141028     C                   PARM                    OutMsg
101400141028     C                   MOVEA     wSkParam      SkSplitCSV_5
101500141028     C*
101600141028     C                   ENDSR
101700141028
101800141028
101900141028     C*----------------------------------------------------*
102000141028     C*  SCRITTURA BUFFER EDIVAT
102100141028     C*----------------------------------------------------*
102200141028     C     WRIVAT        BEGSR
102300141028     C*
102400141028     C                   IF        VATNOT <> *blanks
102500141028     C                   EVAL      VATFGS = VABFGS
102600141028     C                   EVAL      VATCCM = VABCCM
102700141028     C                   EVAL      VATLNP = VABLNP
102800141028     C                   EVAL      VATAAS = VABAAS
102900141028     C                   EVAL      VATNRS = VABNRS
103000141028     C                   EVAL      VATNSP = VABNSP
103100141028     C                   EVAL      VATCMR = VABCMR
103200141028     C                   EVAL      VATCNT = VABCNT
103300141028     C                   ADD       1             §CTROKVT
103400141028     C                   WRITE     EDIVAT00
103500141028     C                   ENDIF
103600141028     C*
103700141028     C                   ENDSR
103800000801
103900011113
104000011113     C*----------------------------------------------------*
104100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
104200011113     C*----------------------------------------------------*
104300011113     C     CHKIMPDIV     BEGSR
104400011113     C*
104500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
104600011113     C                   Z-ADD     *zeros        wrkDec            9 9
104700011113     C*
104800011113     C* Come prima cosa effettuo considerazioni sulla divisa
104900011113     C                   IF        vabIAS > *zeros
105000011113     C                   IF        vabVAS <> 'EUR'
105100011113     C                   EVAL      vabVAS =  'ITL'
105200011113     C                   ENDIF
105300011113     C                   ENDIF
105400011113     C*
105500011113     C                   IF        vabCAS > *zeros
105600011113     C                   IF        vabVCA <> 'EUR'
105700011113     C                   EVAL      vabVCA =  'ITL'
105800011113     C                   ENDIF
105900011113     C                   ENDIF
106000011113     C*
106100011113     C                   IF        vabVMD > *zeros
106200020305     C                   IF        vabVAD <> 'EUR'
106300011113     C                   EVAL      vabVAD =  'ITL'
106400011113     C                   ENDIF
106500011113     C                   ENDIF
106600011113     C*
106700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
106800011113     C                   Z-ADD     vabIAS        wrkDec
106900011113     C                   IF        wrkDec > *zeros
107000011113     C                   IF        vabVAS = 'ITL'
107100011113     C                   EVAL      vabIAS = *zeros
107200011113     C                   ENDIF
107300011113     C                   ENDIF
107400011113     C*
107500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
107600011113     C                   Z-ADD     vabCAS        wrkDec
107700011113     C                   IF        wrkDec > *zeros
107800011113     C                   IF        vabVCA = 'ITL'
107900011113     C                   EVAL      vabCAS = *zeros
108000011113     C                   ENDIF
108100011113     C                   ENDIF
108200011113     C*
108300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
108400011113     C                   Z-ADD     vabVMD        wrkDec
108500011113     C                   IF        wrkDec > *zeros
108600011113     C                   IF        vabVAD = 'ITL'
108700011113     C                   EVAL      vabVMD = *zeros
108800011113     C                   ENDIF
108900011113     C                   ENDIF
109000011113     C*
109100011113     C                   ENDSR
109200011113     C***
109300011113
109400011113
109500000801
109600000801
109700990920      /TITLE Invio dei dati al punto operativo.
109800010202     C     invio         BEGSR
109900990920     C*
110000140117     C* 1° invio EDIVAT
110100010201     C                   reset                   dscmz
110200010201     C                   move      vlrpoi        cmzdst
110300140117     C                   eval      cmzfld = 'EDIVATWR'
110400010201     C                   eval      cmzmbd = vlrhdl
110500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
110600021009     C***                if        prmfir = *blanks
110700140117     C                   eval      cmzfla = 'EDIVAT0F'
110800140117     C                   eval      cmzmba = 'EDIVAT0F'
110900021009     C***                else
111000021009     C***                eval      cmzfla = prmfir
111100021009     C***                eval      cmzmba = prmfir
111200021009     C***                endif
111300010201     C                   eval      cmznrr = *zeros
111400020305     C                   move      §ctrokvt      cmznrr
111500021018     C                   eval      cmzlba = vlrfl1
111600010201     C                   call(e)   'TIS711C'
111700010201     C                   parm                    dscmz
111800010201     C                   parm      *blanks       esito
111900010205     C                   if        %error
112000010205     C                             or cmzerr = '1'
112100010205     C                             or esito  = '1'
112200010205     C                   eval      wrkesito = '3'
112300010205     C                   else
112400010201     C*
112500140117     C* 2° invio EDIVAB
112600010201     C                   reset                   dscmz
112700010201     C                   move      vlrpoi        cmzdst
112800010201     C                   eval      cmzfld = vlrfou
112900010201     C                   eval      cmzmbd = vlrhdl
113000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
113100021009     C***                if        prmfir = *blanks
113200140117     C                   eval      cmzfla = 'EDIVAB0F'
113300140117     C                   eval      cmzmba = 'EDIVAB0F'
113400021009     C***                else
113500021009     C***                eval      cmzfla = prmfir
113600021009     C***                eval      cmzmba = prmfir
113700021009     C***                endif
113800010201     C                   eval      cmznrr = *zeros
113900010201     C                   move      §ctrokvb      cmznrr
114000021018     C                   eval      cmzlba = vlrfl1
114100010201     C                   call(e)   'TIS711C'
114200010201     C                   parm                    dscmz
114300010201     C                   parm      *blanks       esito
114400010201     C                   if        %error
114500010201     C                             or cmzerr = '1'
114600010201     C                             or esito  = '1'
114700010201     C                   eval      wrkesito = '3'
114800010201     C                   endif
114900010205     C                   endif
115000990920     C*
115100000613     C                   ENDSR
115200000613     C***
115300070411
115400141028
115500070411     C     *pssr         BEGSR
115600070411     C*
115700070411     C                   if        %open(tivin00r)
115800070411     C                   close     tivin00r
115900070411     C                   endif
116000140117     C                   if        %open(edivabwr)
116100140117     C                   close     edivabwr
116200070411     C                   endif
116300140117     C                   if        %open(edivatwr)
116400140117     C                   close     edivatwr
116500070411     C                   endif
116600070411     C*
116700070411     C* Effettuo la chiamata al CLLE preposto
116800140117     C                   call(e)   'TITVEVTC'
116900070411     C                   parm                    parccm
117000070411     C                   parm                    parmbr
117100070411     C                   parm      '2'           paropz
117200070411     C*
117300070411     C                   eval      wrkesito = '2'
117400070411     C*
117500070411     C                   seton                                        LR
117600070411     C*
117700070411     C                   ENDSR     '*CANCL'
117800070411     C***
117900070411
118000990910
118100000613     C     *inzsr        BEGSR
118200990910     C*
118300990910     C     *entry        plist
118400990920     C                   parm                    tivlrds
118500990921     C                   parm      wrkesito      esito
118600000724     C                   parm                    prmlit
118700000710     C                   parm                    prmfir
118800000613     C*
118900000830     C* CALCOLA LA DATA CORRENTE
119000140117     C                   time                    wn14             14 0
119100140117     C                   movel     wn14          oracor            6 0          *ORA
119200100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
119300100722     C                   eval      datcor = %dec(%date() : *ISO)
119400000830     C*
119500000613     C                   ENDSR
119600000613     C***
