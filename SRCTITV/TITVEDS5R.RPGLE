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
013800141028     C                   EXSR      INZVAR
013900140117     C*
014000140902     C                   if        *in53
014100140117     C                   call      'TIS7P2SER'
014200140117     C                   parm      'O'           pIn_Opz           1
014300140117     C                   parm                    tivlrds
014400140117     C                   parm                    EDIVABDS
014500140117     C                   parm      *blanks       pIn_CdIdAz        2
014600140117     C                   parm      *blanks       pIn_MaskPDF      50
014700140117     C                   parm      *blanks       pOut_Esito        1
014800140902     C                   endif
014900130708     C*
015000921023     C                   DO        *HIVAL
015100990913     C*
015200990915     C                   READ      tivin00r                               70
015300040910     C                   if        vindta > *blanks
015400000613     C                   add       1             rrnum
015500000801     C*
015600000801     C                   if        *in70 = *off
015700000801     C                             and
015800000801     C                             (vinflg = *blanks
015900000801     C                              or vinflg = '0'
016000000801     C                              or vinflg = '2')
016100000801     C*
016200000801     C                   clear                   vinmsg
016300141028     C*
016400141028     C* "normalizzo" il dato di input
016500141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
016600141028     C                                             %len(%trim(vindta))-1))
016700160209     C*
016800160209     C* Elimino le "duple di controllo EDIFACT"
016900160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017000160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
017100160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
017200160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
017300160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
017400040910     C*
017500040910     C* Eseguo routine d traduzione
017600040910     C                   exsr      impvabvat
017700040802     C*
017800010305     C                   endif
017900000905     C*
018000000905     C                   else
018100000905     C                   eval      vinflg = '1'
018200000905     C                   endif
018300000905     C*
018400000905     C  N70              update    tivin000
018500000905     C*
018600991022     C  N70              ENDdo
018700100722     C*
018800100722     C* Scarico i buffer testata ancora "in canna"
018900140922     C*
019000141028     C***  N31              ADD       1             §CTROKVB
019100141028     C***  N31              exsr      CHKWRI
019200141028     C***  N31              WRITE     EDIVAB00
019300010202     C*
019400010202     C                   endif
019500990910
019600990910     C* Se non ci sono record con errori ...
019700000710     C                   if        §ctrno = 0
019800990910     C* ... restituisco esito OK.
019900990921     C                   eval      wrkesito = '0'
020000990910     C                   else
020100010201     C                   if        §ctrokvb > 0
020200990921     C                   eval      wrkesito = '1'
020300000710     C                   else
020400000710     C                   eval      wrkesito = '2'
020500990910     C                   endif
020600000710     C                   endif
020700990910     C*
020800990914     C                   if        %open(tivin00r)
020900990908     C                   close     tivin00r
021000990914     C                   endif
021100140117     C                   if        %open(edivabwr)
021200140117     C                   close     edivabwr
021300990914     C                   endif
021400140117     C                   if        %open(edivatwr)
021500140117     C                   close     edivatwr
021600010201     C                   endif
021700990910     C*
021800010201     C                   if        §ctrokvb > 0
021900000724     C                             and vlrpoi <> *zeros
022000010202     C                   exsr      invio
022100990920     C                   endif
022200990920     C*
022300910830     C                   ENDSR
022400000613     C***
022500140922
022600140922     C*----------------------------------------------------*
022700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
022800140922     C*----------------------------------------------------*
022900140922     C     CHKWRI        BEGSR
023000140922     C*
023100141028     C                   if        VABRMN = *zeros
023200141028     C                   eval      VABRMN = VABNSP
023300141028     C                   endif
023400141028     C*
023500141028     C                   if        VABPRD = *blanks
023600141028     C* ......VABPRD
023700141028     C* Reperisco la provincia dal CAP e dalla località
023800141028     C                   IF        VABLOD <> *blanks AND
023900141028     C                             VABCAD <> *blanks AND
024000141028     C                             VABNZD  = *blanks
024100141028     C                   CLEAR                   TISI95DS
024200141028     C                   EVAL      I95TCN = '3'
024300141028     C                   Z-ADD     datcor        I95DAT
024400141028     C                   EVAL      I95CAP = VABCAD
024500141028     C                   EVAL      I95LOC = VABLOD
024600141028     C                   CALL      'TISI95R'
024700141028     C                   PARM                    TISI95DS
024800141028     C                   EVAL      VABPRD = O95PRV
024900141028     C                   ENDIF
025000141028     C                   endif
025100140922     C*
025200140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
025300141028     C                   IF        wFlgCAS <> '0'
025400140922     C                   IF        VABCBO = '1'
025500140922     C                   EVAL      VABCBO = '4'
025600140922     C                   ELSE
025700140922     C                   EVAL      VABCBO = '6'
025800140922     C                   ENDIF
025900140922     C                   ENDIF
026000140922     C*
026100140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
026200141028     C                   EXSR      CHKIMPDIV
026300140922     C*
026400140922     C                   ENDSR
026500141028     C*----------------------------------------------------*
026600141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026700141028     C*----------------------------------------------------*
026800141028     C     INZVAR        BEGSR
026900141028     C*
027000141028     C* Inizializzo variabili di wrk
027100141028     C                   Z-ADD     *zeros        Num5_0            5 0
027200141028     C                   MOVEL     '0'           wFlgCAS
027300141028     C                   MOVEL     *blanks       wVATNOT_A
027400141028     C                   MOVEL     *blanks       wVATNOT_B
027500141028     C                   MOVEL     *blanks       wVATNOT_E
027600141028     C                   MOVEL     *blanks       wVATNOT_I
027700141028     C                   MOVEL     *blanks       wVATNOT_J
027800141028     C                   MOVEL     *blanks       wVATNOT_S
027900141028     C                   MOVEL     *blanks       wVATNOT_P
028000141028     C                   CLEAR                   SkSplitCSV_5
028100141028     C*
028200141028     C* Reimposto i valori di default
028300141028     C                   EXSR      DEFCAM
028400141028     C*
028500141028     C                   ENDSR
028600141028     C*----------------------------------------------------*
028700141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028800141028     C*----------------------------------------------------*
028900141028     C     DEFCAM        BEGSR
029000141028     C*
029100141028     C                   CLEAR                   EDIVAB00
029200141028     C                   CLEAR                   EDIVAT00
029300141028     C*
029400141028     C* Imposto i valori bolla di default
029500141028     C                   EVAL      VABCTR = 000
029600141028     C                   EVAL      VABCBO = '1'
029700141104     C                   EVAL      VABTSP = 'C'
029800141028     C*
029900141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030000141028     C* e delimitatore testo.
030100141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030200141028     C*
030300141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030400141028     C                   EVAL      CharSOS = CharNUM
030500141028     C*
030600141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
030700141028     C                   SETOFF                                       53
030800141028     C                   SELECT
030900141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
031000141028     C                   SETON                                        53
031100141028     C                   ENDSL
031200141028     C*
031300141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031400141028     C                   EVAL      posDaDft = 1
031500141028     C                   EVAL      posADft  = 0
031600141028     C                   EVAL      wGiroDft = 0
031700141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031800141028     C                             posDaDft > 0
031900141028     C*
032000141028     C* Gestisco il 1° giro
032100141028     C                   IF        wGiroDft = 0
032200141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
032300141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032400141028     C* Incremento il contatore dei "giri"
032500141028     C                   EVAL      wGiroDft = 1
032600141028     C                   ELSE
032700141028     C                   EVAL      posDaDft = posADft
032800141028     C                   ENDIF
032900141028     C* Eseguo lo scan x trovare la fine del campo corrente
033000141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033100141028     C*
033200141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
033300141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
033400141028     C                   IF        posDaDft > 0 AND
033500141028     C                             posADft  > 0
033600141028     C* NCL
033700141028     C                   IF        %subst(
033800141028     C                             %subst(vlrppt:posDaDft+1:
033900141028     C                             posADft-posDaDft-1):1:3)
034000141028     C                             = 'NCL'
034100141028     C                   EVAL      PiStr=%trim(%subst(
034200141028     C                             %subst(vlrppt:posDaDft+1:
034300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034400141028     C                   EXSR      CHKNUM
034500141028     C                   IF        PiInt=*on
034600141028     C                   Z-ADD     PiVal         VABNCL
034700141028     C                   ENDIF
034800141028     C                   ENDIF
034900141028     C* CCM
035000141028     C                   IF        %subst(
035100141028     C                             %subst(vlrppt:posDaDft+1:
035200141028     C                             posADft-posDaDft-1):1:3)
035300141028     C                             = 'CCM'
035400141028     C                   EVAL      PiStr=%trim(%subst(
035500141028     C                             %subst(vlrppt:posDaDft+1:
035600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035700141028     C                   EXSR      CHKNUM
035800141028     C                   IF        PiInt=*on
035900141028     C                   Z-ADD     PiVal         VABCCM
036000141028     C                   ENDIF
036100141028     C                   ENDIF
036200141028     C* LNP
036300141028     C                   IF        %subst(
036400141028     C                             %subst(vlrppt:posDaDft+1:
036500141028     C                             posADft-posDaDft-1):1:3)
036600141028     C                             = 'LNP'
036700141028     C                   EVAL      PiStr=%trim(%subst(
036800141028     C                             %subst(vlrppt:posDaDft+1:
036900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037000141028     C                   EXSR      CHKNUM
037100141028     C                   IF        PiInt=*on
037200141028     C                   Z-ADD     PiVal         VABLNP
037300141028     C                   ENDIF
037400141028     C                   ENDIF
037500141028     C* NRS
037600141028     C                   IF        %subst(
037700141028     C                             %subst(vlrppt:posDaDft+1:
037800141028     C                             posADft-posDaDft-1):1:3)
037900141028     C                             = 'NRS'
038000141028     C                   EVAL      PiStr=%trim(%subst(
038100141028     C                             %subst(vlrppt:posDaDft+1:
038200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038300141028     C                   EXSR      CHKNUM
038400141028     C                   IF        PiInt=*on
038500141028     C                   Z-ADD     PiVal         VABNRS
038600141028     C                   ENDIF
038700141028     C                   ENDIF
038800141028     C* CTR
038900141028     C                   IF        %subst(
039000141028     C                             %subst(vlrppt:posDaDft+1:
039100141028     C                             posADft-posDaDft-1):1:3)
039200141028     C                             = 'CTR'
039300141028     C                   EVAL      PiStr=%trim(%subst(
039400141028     C                             %subst(vlrppt:posDaDft+1:
039500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039600141028     C                   EXSR      CHKNUM
039700141028     C                   IF        PiInt=*on
039800141028     C                   Z-ADD     PiVal         VABCTR
039900141028     C                   ENDIF
040000141028     C                   ENDIF
040100141028     C* PKB
040200141028     C                   IF        %subst(
040300141028     C                             %subst(vlrppt:posDaDft+1:
040400141028     C                             posADft-posDaDft-1):1:3)
040500141028     C                             = 'PKB'
040600141028     C                   EVAL      PiStr=%trim(%subst(
040700141028     C                             %subst(vlrppt:posDaDft+1:
040800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900141028     C                   EXSR      CHKNUM
041000141028     C                   IF        PiNum=*on
041100141028     C                   Z-ADD     PiVal         VABPKB
041200141028     C                   ENDIF
041300141028     C                   ENDIF
041400141028     C* VLB
041500141028     C                   IF        %subst(
041600141028     C                             %subst(vlrppt:posDaDft+1:
041700141028     C                             posADft-posDaDft-1):1:3)
041800141028     C                             = 'VLB'
041900141028     C                   EVAL      PiStr=%trim(%subst(
042000141028     C                             %subst(vlrppt:posDaDft+1:
042100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200141028     C                   EXSR      CHKNUM
042300141028     C                   IF        PiNum=*on
042400141028     C                   Z-ADD     PiVal         VABVLB
042500141028     C                   ENDIF
042600141028     C                   ENDIF
042700141028     C* QFT
042800141028     C                   IF        %subst(
042900141028     C                             %subst(vlrppt:posDaDft+1:
043000141028     C                             posADft-posDaDft-1):1:3)
043100141028     C                             = 'QFT'
043200141028     C                   EVAL      PiStr=%trim(%subst(
043300141028     C                             %subst(vlrppt:posDaDft+1:
043400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500141028     C                   EXSR      CHKNUM
043600141028     C                   IF        PiNum=*on
043700141028     C                   Z-ADD     PiVal         VABQFT
043800141028     C                   ENDIF
043900141028     C                   ENDIF
044000141028     C* CBO
044100141028     C                   IF        %subst(
044200141028     C                             %subst(vlrppt:posDaDft+1:
044300141028     C                             posADft-posDaDft-1):1:3)
044400141028     C                             = 'CBO'
044500141028     C                   EVAL      VABCBO=%trim(%subst(
044600141028     C                             %subst(vlrppt:posDaDft+1:
044700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800141028     C                   ENDIF
044900141028     C* TSP
045000141028     C                   IF        %subst(
045100141028     C                             %subst(vlrppt:posDaDft+1:
045200141028     C                             posADft-posDaDft-1):1:3)
045300141028     C                             = 'TSP'
045400141028     C                   EVAL      VABTSP=%trim(%subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045700141028     C                   ENDIF
045800141028     C* VAS
045900141028     C                   IF        %subst(
046000141028     C                             %subst(vlrppt:posDaDft+1:
046100141028     C                             posADft-posDaDft-1):1:3)
046200141028     C                             = 'VAS'
046300141028     C                   EVAL      VABVAS=%trim(%subst(
046400141028     C                             %subst(vlrppt:posDaDft+1:
046500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046600141028     C                   ENDIF
046700141028     C* VCA
046800141028     C                   IF        %subst(
046900141028     C                             %subst(vlrppt:posDaDft+1:
047000141028     C                             posADft-posDaDft-1):1:3)
047100141028     C                             = 'VCA'
047200141028     C                   EVAL      VABVCA=%trim(%subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047500141028     C                   ENDIF
047600141028     C* TIC
047700141028     C                   IF        %subst(
047800141028     C                             %subst(vlrppt:posDaDft+1:
047900141028     C                             posADft-posDaDft-1):1:3)
048000141028     C                             = 'TIC'
048100141028     C                   EVAL      VABTIC=%trim(%subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400141028     C                   ENDIF
048500141028     C* GCA
048600141028     C                   IF        %subst(
048700141028     C                             %subst(vlrppt:posDaDft+1:
048800141028     C                             posADft-posDaDft-1):1:3)
048900141028     C                             = 'GCA'
049000141028     C                   EVAL      VABGCA=%trim(%subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300141028     C                   ENDIF
049400141028     C* CTM
049500141028     C                   IF        %subst(
049600141028     C                             %subst(vlrppt:posDaDft+1:
049700141028     C                             posADft-posDaDft-1):1:3)
049800141028     C                             = 'CTM'
049900141028     C                   EVAL      VABCTM=%trim(%subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200141028     C                   ENDIF
050300141028     C* FFD
050400141028     C                   IF        %subst(
050500141028     C                             %subst(vlrppt:posDaDft+1:
050600141028     C                             posADft-posDaDft-1):1:3)
050700141028     C                             = 'FFD'
050800141028     C                   EVAL      VABFFD=%trim(%subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100141028     C                   ENDIF
051200141028     C* VAD
051300141028     C                   IF        %subst(
051400141028     C                             %subst(vlrppt:posDaDft+1:
051500141028     C                             posADft-posDaDft-1):1:3)
051600141028     C                             = 'VAD'
051700141028     C                   EVAL      VABVAD=%trim(%subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000141028     C                   ENDIF
052100141028     C* GMA
052200141028     C                   IF        %subst(
052300141028     C                             %subst(vlrppt:posDaDft+1:
052400141028     C                             posADft-posDaDft-1):1:3)
052500141028     C                             = 'GMA'
052600141028     C                   EVAL      VABGMA=%trim(%subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900141028     C                   ENDIF
053000141028     C* GGA
053100141028     C                   IF        %subst(
053200141028     C                             %subst(vlrppt:posDaDft+1:
053300141028     C                             posADft-posDaDft-1):1:3)
053400141028     C                             = 'GGA'
053500141028     C                   EVAL      VABGGA=%trim(%subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800141028     C                   ENDIF
053900141028     C* GVA
054000141028     C                   IF        %subst(
054100141028     C                             %subst(vlrppt:posDaDft+1:
054200141028     C                             posADft-posDaDft-1):1:3)
054300141028     C                             = 'GVA'
054400141028     C                   EVAL      VABGVA=%trim(%subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700141028     C                   ENDIF
054800141028     C* TC1
054900141028     C                   IF        %subst(
055000141028     C                             %subst(vlrppt:posDaDft+1:
055100141028     C                             posADft-posDaDft-1):1:3)
055200141028     C                             = 'TC1'
055300141028     C                   EVAL      VABTC1=%trim(%subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600141028     C                   ENDIF
055700141028     C* TC2
055800141028     C                   IF        %subst(
055900141028     C                             %subst(vlrppt:posDaDft+1:
056000141028     C                             posADft-posDaDft-1):1:3)
056100141028     C                             = 'TC2'
056200141028     C                   EVAL      VABTC2=%trim(%subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500141028     C                   ENDIF
056600141028     C* VATTRC
056700141028     C                   IF        %subst(
056800141028     C                             %subst(vlrppt:posDaDft+1:
056900141028     C                             posADft-posDaDft-1):1:3)
057000141028     C                             = 'TRC'
057100141028     C                   EVAL      VATTRC=%trim(%subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400141028     C                   ENDIF
057500141028     C* ...
057600141028     C                   ENDIF
057700141028     C                   ENDDO
057800141028     C*
057900141028     C                   ENDSR
058000000801     C*----------------------------------------------------*
058100140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
058200000801     C*----------------------------------------------------*
058300040910     C     IMPVABVAT     BEGSR
058400040910     C*
058500040910     C* Traduzione relativa ai tipi record del file del cliente
058600040910     C*
058700071210     C*
058800071210     C***
058900141028     C* ...tipo record 'UNB' (info CMR)
059000141028     C                   EVAL      wTag = 'UNB'
059100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
059200141028     C                   MOVEL     *blanks       savCMR           35
059300141028     C                   EVAL      InStringa =
059400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
059500141028     C                   EVAL      InSepara = '+'
059600141028     C                   EXSR      SR_SPLIT
059700141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
059800140922     C                                      %char(datcor) + ' ' + %char(oracor)
059900130620     C                   ENDIF
060000141028     C*
060100141028     C***
060200141028     C* ...tipo record 'UNH' (rottuta di codice spedizione)
060300141028     C                   EVAL      wTag = 'UNH'
060400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
060500141028     C                   SETOFF                                       31
060600141028     C                   eval      vinflg = '1'
060700141028     C* ......inizializzazioni iniziali e formati record file Bartolini
060800141028     C                   EXSR      INZVAR
060900141028     C* ......valorizzazione campi fissi
061000141028     C                   MOVEL     datcor        VABAAS
061100141028     C                   MOVE      datcor        VABMGS
061200141028     C                   MOVE(P)   vlrpoi        VABFGS
061300141028     C                   EVAL      VABCMR = savCMR
061400141028     C                   EVAL      VABDCM = datcor
061500141028     C                   EVAL      VABDTS = datcor
061600141028     C                   EVAL      VABCNT = 1
061700141028     C* ......NSP => Stacco un numeratore da AZNUM
061800141028     C                   clear                   TRUL33DS
061900141028     C                   eval      I33OPE = *zeros
062000141028     C                   eval      I33CNU = 302
062100141028     C                   eval      I33NUM = 1
062200141028     C                   movel     TRUL33DS      KPJBU
062300141028     C                   call      'TRUL33R'
062400141028     C                   parm                    KPJBA
062500141028     C                   movel     KPJBU         TRUL33DS
062600141028     C                   if        O33ERR = *zeros
062700141028     C                   z-add     O33NRF        VABNSP
062800141028     C                   else
062900141028     C                   Z-ADD     1             wRecErr
063000141028     C                   SETON                                        31
063100141028     C                   EVAL      vinmsg = %trimr(vinmsg)
063200141028     C                             + ' ' + 'VABNSP VATNSP'
063300141028     C                   endif
063400141028     C                   ENDIF
063500141028     C*
063600141028     C***
063700141028     C* ...tipo record 'RFF+DQ' (riferimento spedizione)
063800141028     C                   EVAL      wTag = 'RFF+DQ'
063900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064000141028     C                   EVAL      VABRMA =
064100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
064200141028     C                   ENDIF
064300151102     C*
064400151102     C***
064500151102     C* ...tipo record 'DTM+69' (data consegna richiesta)
064600151102     C                   EVAL      wTag = 'DTM+69'
064700151102     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064800151102     C                   EVAL      PiStr=%subst(
064900151102     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
065000151102     C                   EXSR      CHKNUM
065100151102     C                   IF        PiInt=*on
065200151102     C                   Z-ADD     PiVal         VABDCR
065300151102     C                   ELSE
065400151102     C                   Z-ADD     1             wRecErr
065500151102     C                   ENDIF
065600151102     C                   ENDIF
065700141028     C*
065800141028     C***
065900141028     C* ...tipo record 'NAD+CN' (destinatario)
066000141028     C                   EVAL      wTag = 'NAD+CN'
066100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066200141028     C                   EVAL      InStringa =
066300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
066400141028     C                   EVAL      InSepara = '+'
066500141028     C                   EXSR      SR_SPLIT
066600141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
066700141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
066800141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
066900141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
067000141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
067100141028     C                   IF        VABNZD = 'I'   OR
067200141028     C                             VABNZD = 'IT'  OR
067300141028     C                             VABNZD = 'ITA'
067400141028     C                   EVAL      VABNZD = *blanks
067500141028     C                   ENDIF
067600141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
067700141028     C                   EVAL      InSepara = ':'
067800141028     C                   EXSR      SR_SPLIT
067900141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
068000141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
068100141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
068200141028     C                                      %trim(SkSplitCSV_5(4))+
068300141028     C                                      %trim(SkSplitCSV_5(5))
068400141028     C                   ENDIF
068500141104     C*
068600141104     C***
068700141104     C* ...tipo record 'NAD+UC' (destinatario)
068800141104     C                   EVAL      wTag = 'NAD+UC'
068900141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069000141104     C                   EVAL      InStringa =
069100141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
069200141104     C                   EVAL      InSepara = '+'
069300141104     C                   EXSR      SR_SPLIT
069400141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
069500141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
069600141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
069700141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
069800141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
069900141104     C                   IF        VABNZD = 'I'   OR
070000141104     C                             VABNZD = 'IT'  OR
070100141104     C                             VABNZD = 'ITA'
070200141104     C                   EVAL      VABNZD = *blanks
070300141104     C                   ENDIF
070400141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
070500141104     C                   EVAL      InSepara = ':'
070600141104     C                   EXSR      SR_SPLIT
070700141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
070800141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
070900141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
071000141104     C                                      %trim(SkSplitCSV_5(4))+
071100141104     C                                      %trim(SkSplitCSV_5(5))
071200141104     C                   ENDIF
071300141028     C*
071400141028     C***
071500141028     C* ...tipo record 'GID' (colli)
071600141028     C                   EVAL      wTag = 'GID'
071700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071800141028     C                   EVAL      InStringa =
071900141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
072000141028     C                   EVAL      InSepara = '+'
072100141028     C                   EXSR      SR_SPLIT
072200141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
072300141028     C                   EXSR      CHKNUM
072400141028     C                   IF        PiInt=*on
072500141028     C                   Z-ADD     PiVal         VABNCL
072600141028     C                   ELSE
072700141028     C                   Z-ADD     1             wRecErr
072800141028     C                   ENDIF
072900141028     C                   ENDIF
073000141028     C*
073100141028     C***
073200141028     C* ...tipo record 'FTX+AAA' (natura merce)
073300141028     C                   EVAL      wTag = 'FTX+AAA'
073400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073500141028     C                   EVAL      InStringa =
073600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
073700141028     C                   EVAL      InSepara = '+'
073800141028     C                   EXSR      SR_SPLIT
073900141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
074000141028     C                   ENDIF
074100141028     C*
074200141028     C***
074300141028     C* ...tipo record 'MEA+WT+AAE' (peso spedizione)
074400141028     C                   EVAL      wTag = 'MEA+WT+AAE'
074500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074600141028     C                   EVAL      InStringa =
074700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
074800141028     C                   EVAL      InSepara = ':'
074900141028     C                   EXSR      SR_SPLIT
075000141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
075100141028     C                   EXSR      CHKNUM
075200141028     C                   IF        PiNum=*on
075300150924     C                   ADD       PiVal         VABPKB
075400141028     C                   ELSE
075500141028     C                   Z-ADD     1             wRecErr
075600141028     C                   ENDIF
075700141028     C                   ENDIF
075800141028     C*
075900141028     C***
076000141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
076100141028     C                   EVAL      wTag = 'MEA+VOL'
076200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076300141028     C                   EVAL      InStringa =
076400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
076500141028     C                   EVAL      InSepara = ':'
076600141028     C                   EXSR      SR_SPLIT
076700141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
076800141028     C                   EXSR      CHKNUM
076900141028     C                   IF        PiNum=*on
077000150924     C                   ADD       PiVal         VABVLB
077100141028     C                   ELSE
077200141028     C                   Z-ADD     1             wRecErr
077300141028     C                   ENDIF
077400141028     C                   ENDIF
077500141028     C*
077600141028     C***
077700141028     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
077800141028     C                   EVAL      wTag = 'GIN+BJ'
077900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078000141028     C                   EVAL      VATNOT =
078100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
078200141028     C                   EVAL      VATTRC = 'E'
078300141028     C                   EXSR      WRIVAT
078400141028     C                   ENDIF
078500141028     C*
078600141028     C***
078700141028     C* ...tipo record '???' (PDF per packing-list)
078800141028     C                   EVAL      wTag = '???PDF'
078900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079000141028     C*
079100141028     C* ......VATNOT_P
079200141028     C                   if        *in53
079300141028     C*
079400141028     C* Solo se valorizzato RMN
079500141028     C                   if        VABRMA <> *blanks
079600141028     C                   eval      pIn_MaskPDF =
079700141028     C                                     %trim(%subst(wvindta:1292:100))
079800141028     C                   call      'TIS7P2SER'
079900141028     C                   parm      'W'           pIn_Opz           1
080000141028     C                   parm                    tivlrds
080100141028     C                   parm                    EDIVABDS
080200141028     C                   parm      '10'          pIn_CdIdAz        2
080300141028     C                   parm                    pIn_MaskPDF      50
080400141028     C                   parm      *blanks       pOut_Esito        1
080500141028     C                   endif
080600141028     C                   endif
080700141028     C*
080800141028     C                   ENDIF
080900141028     C*
081000141028     C***
081100141028     C* ...tipo record 'UNT' (fine bolla)
081200141028     C                   EVAL      wTag = 'UNT'
081300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081400141028     C  N31              ADD       1             §CTROKVB
081500141028     C  N31              exsr      CHKWRI
081600140117     C  N31              WRITE     EDIVAB00
081700100722     C                   endif
081800010202     C*
081900000801     C* Ebbene...
082000000801     C                   ADD       1             §CTRMO
082100141028     C                   IF        wRecErr <> *zeros
082200000801     C                   ADD       1             §CTRNO
082300000801     C                   EVAL      vinflg = '2'
082400000801     C                   ENDIF
082500000801     C*
082600000801     C                   ENDSR
082700010202     C*----------------------------------------------------*
082800140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
082900010202     C*----------------------------------------------------*
083000020305     C     PREVAT        BEGSR
083100010202     C*
083200140117     C* Compongo il nome del membro da dare al EDIVATWR
083300010202     C                   eval      parmbr = vlrhdl
083400010202     C                   movel     'M'           parmbr
083500060113     C                   eval      parccm = vlrksc
083600010202     C                   eval      paropz = '1'
083700010202     C* Effettuo la chiamata al CLLE preposto
083800140117     C                   call(e)   'TITVEVTC'
083900010202     C                   parm                    parccm
084000010202     C                   parm                    parmbr
084100010202     C                   parm                    paropz
084200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
084300010202     C                   if        %error
084400010202     C                   movel     '1'           chkcall
084500010202     C                   else
084600010202     C                   movel     '0'           chkcall
084700010202     C                   endif
084800010202     C*
084900010202     C                   ENDSR
085000141028     C*----------------------------------------------------*
085100141028     C*  CONTROLLO NUMERICITA' CAMPI
085200141028     C*----------------------------------------------------*
085300141028     C     CHKNUM        BEGSR
085400141028     C*
085500141028     C                   IF        PiDecChr = *blanks
085600141028     C                   EVAL      PiDecChr = CharNUM
085700141028     C                   ENDIF
085800141028     C*
085900141028     C                   callp     UBISNUM_Check(PiStr
086000141028     C                                          :PiDecChr
086100141028     C                                          :PiVal
086200141028     C                                          :PiNum
086300141028     C                                          :PiInt)
086400141028     C*
086500141028     C                   ENDSR
086600141028     C***
086700141028
086800141028
086900141028     C*----------------------------------------------------*
087000141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
087100141028     C*----------------------------------------------------*
087200141028     C     SR_SPLIT      BEGSR
087300141028     C*
087400141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
087500141028     C                   CALL      'XSPLIT2'
087600141028     C                   PARM                    InStringa
087700141028     C                   PARM                    InSepara
087800141028     C                   PARM      '5'           InTypeOut
087900141028     C                   PARM      ''            wSkParam
088000141028     C                   PARM                    OutErrore
088100141028     C                   PARM                    OutMsg
088200141028     C                   MOVEA     wSkParam      SkSplitCSV_5
088300141028     C*
088400141028     C                   ENDSR
088500141028
088600141028
088700141028     C*----------------------------------------------------*
088800141028     C*  SCRITTURA BUFFER EDIVAT
088900141028     C*----------------------------------------------------*
089000141028     C     WRIVAT        BEGSR
089100141028     C*
089200141028     C                   IF        VATNOT <> *blanks
089300141028     C                   EVAL      VATFGS = VABFGS
089400141028     C                   EVAL      VATCCM = VABCCM
089500141028     C                   EVAL      VATLNP = VABLNP
089600141028     C                   EVAL      VATAAS = VABAAS
089700141028     C                   EVAL      VATNRS = VABNRS
089800141028     C                   EVAL      VATNSP = VABNSP
089900141028     C                   EVAL      VATCMR = VABCMR
090000141028     C                   EVAL      VATCNT = VABCNT
090100141028     C                   ADD       1             §CTROKVT
090200141028     C                   WRITE     EDIVAT00
090300141028     C                   ENDIF
090400141028     C*
090500141028     C                   ENDSR
090600000801
090700011113
090800011113     C*----------------------------------------------------*
090900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
091000011113     C*----------------------------------------------------*
091100011113     C     CHKIMPDIV     BEGSR
091200011113     C*
091300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
091400011113     C                   Z-ADD     *zeros        wrkDec            9 9
091500011113     C*
091600011113     C* Come prima cosa effettuo considerazioni sulla divisa
091700011113     C                   IF        vabIAS > *zeros
091800011113     C                   IF        vabVAS <> 'EUR'
091900011113     C                   EVAL      vabVAS =  'ITL'
092000011113     C                   ENDIF
092100011113     C                   ENDIF
092200011113     C*
092300011113     C                   IF        vabCAS > *zeros
092400011113     C                   IF        vabVCA <> 'EUR'
092500011113     C                   EVAL      vabVCA =  'ITL'
092600011113     C                   ENDIF
092700011113     C                   ENDIF
092800011113     C*
092900011113     C                   IF        vabVMD > *zeros
093000020305     C                   IF        vabVAD <> 'EUR'
093100011113     C                   EVAL      vabVAD =  'ITL'
093200011113     C                   ENDIF
093300011113     C                   ENDIF
093400011113     C*
093500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
093600011113     C                   Z-ADD     vabIAS        wrkDec
093700011113     C                   IF        wrkDec > *zeros
093800011113     C                   IF        vabVAS = 'ITL'
093900011113     C                   EVAL      vabIAS = *zeros
094000011113     C                   ENDIF
094100011113     C                   ENDIF
094200011113     C*
094300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
094400011113     C                   Z-ADD     vabCAS        wrkDec
094500011113     C                   IF        wrkDec > *zeros
094600011113     C                   IF        vabVCA = 'ITL'
094700011113     C                   EVAL      vabCAS = *zeros
094800011113     C                   ENDIF
094900011113     C                   ENDIF
095000011113     C*
095100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
095200011113     C                   Z-ADD     vabVMD        wrkDec
095300011113     C                   IF        wrkDec > *zeros
095400011113     C                   IF        vabVAD = 'ITL'
095500011113     C                   EVAL      vabVMD = *zeros
095600011113     C                   ENDIF
095700011113     C                   ENDIF
095800011113     C*
095900011113     C                   ENDSR
096000011113     C***
096100011113
096200011113
096300000801
096400000801
096500990920      /TITLE Invio dei dati al punto operativo.
096600010202     C     invio         BEGSR
096700990920     C*
096800140117     C* 1° invio EDIVAT
096900010201     C                   reset                   dscmz
097000010201     C                   move      vlrpoi        cmzdst
097100140117     C                   eval      cmzfld = 'EDIVATWR'
097200010201     C                   eval      cmzmbd = vlrhdl
097300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
097400021009     C***                if        prmfir = *blanks
097500140117     C                   eval      cmzfla = 'EDIVAT0F'
097600140117     C                   eval      cmzmba = 'EDIVAT0F'
097700021009     C***                else
097800021009     C***                eval      cmzfla = prmfir
097900021009     C***                eval      cmzmba = prmfir
098000021009     C***                endif
098100010201     C                   eval      cmznrr = *zeros
098200020305     C                   move      §ctrokvt      cmznrr
098300021018     C                   eval      cmzlba = vlrfl1
098400010201     C                   call(e)   'TIS711C'
098500010201     C                   parm                    dscmz
098600010201     C                   parm      *blanks       esito
098700010205     C                   if        %error
098800010205     C                             or cmzerr = '1'
098900010205     C                             or esito  = '1'
099000010205     C                   eval      wrkesito = '3'
099100010205     C                   else
099200010201     C*
099300140117     C* 2° invio EDIVAB
099400010201     C                   reset                   dscmz
099500010201     C                   move      vlrpoi        cmzdst
099600010201     C                   eval      cmzfld = vlrfou
099700010201     C                   eval      cmzmbd = vlrhdl
099800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
099900021009     C***                if        prmfir = *blanks
100000140117     C                   eval      cmzfla = 'EDIVAB0F'
100100140117     C                   eval      cmzmba = 'EDIVAB0F'
100200021009     C***                else
100300021009     C***                eval      cmzfla = prmfir
100400021009     C***                eval      cmzmba = prmfir
100500021009     C***                endif
100600010201     C                   eval      cmznrr = *zeros
100700010201     C                   move      §ctrokvb      cmznrr
100800021018     C                   eval      cmzlba = vlrfl1
100900010201     C                   call(e)   'TIS711C'
101000010201     C                   parm                    dscmz
101100010201     C                   parm      *blanks       esito
101200010201     C                   if        %error
101300010201     C                             or cmzerr = '1'
101400010201     C                             or esito  = '1'
101500010201     C                   eval      wrkesito = '3'
101600010201     C                   endif
101700010205     C                   endif
101800990920     C*
101900000613     C                   ENDSR
102000000613     C***
102100070411
102200141028
102300070411     C     *pssr         BEGSR
102400070411     C*
102500070411     C                   if        %open(tivin00r)
102600070411     C                   close     tivin00r
102700070411     C                   endif
102800140117     C                   if        %open(edivabwr)
102900140117     C                   close     edivabwr
103000070411     C                   endif
103100140117     C                   if        %open(edivatwr)
103200140117     C                   close     edivatwr
103300070411     C                   endif
103400070411     C*
103500070411     C* Effettuo la chiamata al CLLE preposto
103600140117     C                   call(e)   'TITVEVTC'
103700070411     C                   parm                    parccm
103800070411     C                   parm                    parmbr
103900070411     C                   parm      '2'           paropz
104000070411     C*
104100070411     C                   eval      wrkesito = '2'
104200070411     C*
104300070411     C                   seton                                        LR
104400070411     C*
104500070411     C                   ENDSR     '*CANCL'
104600070411     C***
104700070411
104800990910
104900000613     C     *inzsr        BEGSR
105000990910     C*
105100990910     C     *entry        plist
105200990920     C                   parm                    tivlrds
105300990921     C                   parm      wrkesito      esito
105400000724     C                   parm                    prmlit
105500000710     C                   parm                    prmfir
105600000613     C*
105700000830     C* CALCOLA LA DATA CORRENTE
105800140117     C                   time                    wn14             14 0
105900140117     C                   movel     wn14          oracor            6 0          *ORA
106000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
106100100722     C                   eval      datcor = %dec(%date() : *ISO)
106200000830     C*
106300000613     C                   ENDSR
106400000613     C***
