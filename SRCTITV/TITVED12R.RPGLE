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
023400151102     C*
023500151102     C                   if        VABRMA = *blanks
023600151102     C                   eval      VABRMA = savCMR
023700151102     C                   endif
023800141028     C*
023900141028     C                   if        VABPRD = *blanks
024000141028     C* ......VABPRD
024100141028     C* Reperisco la provincia dal CAP e dalla località
024200141028     C                   IF        VABLOD <> *blanks AND
024300141028     C                             VABCAD <> *blanks AND
024400141028     C                             VABNZD  = *blanks
024500141028     C                   CLEAR                   TISI95DS
024600141028     C                   EVAL      I95TCN = '3'
024700141028     C                   Z-ADD     datcor        I95DAT
024800141028     C                   EVAL      I95CAP = VABCAD
024900141028     C                   EVAL      I95LOC = VABLOD
025000141028     C                   CALL      'TISI95R'
025100141028     C                   PARM                    TISI95DS
025200141028     C                   EVAL      VABPRD = O95PRV
025300141028     C                   ENDIF
025400141028     C                   endif
025500140922     C*
025600140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
025700141028     C                   IF        wFlgCAS <> '0'
025800140922     C                   IF        VABCBO = '1'
025900140922     C                   EVAL      VABCBO = '4'
026000140922     C                   ELSE
026100140922     C                   EVAL      VABCBO = '6'
026200140922     C                   ENDIF
026300140922     C                   ENDIF
026400140922     C*
026500140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
026600141028     C                   EXSR      CHKIMPDIV
026700140922     C*
026800140922     C                   ENDSR
026900141028     C*----------------------------------------------------*
027000141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100141028     C*----------------------------------------------------*
027200141028     C     INZVAR        BEGSR
027300141028     C*
027400141028     C* Inizializzo variabili di wrk
027500141028     C                   Z-ADD     *zeros        Num5_0            5 0
027600141028     C                   MOVEL     '0'           wFlgCAS
027700141028     C                   MOVEL     *blanks       wVATNOT_A
027800141028     C                   MOVEL     *blanks       wVATNOT_B
027900141028     C                   MOVEL     *blanks       wVATNOT_E
028000141028     C                   MOVEL     *blanks       wVATNOT_I
028100141028     C                   MOVEL     *blanks       wVATNOT_J
028200141028     C                   MOVEL     *blanks       wVATNOT_S
028300141028     C                   MOVEL     *blanks       wVATNOT_P
028400141028     C                   CLEAR                   SkSplitCSV_5
028500141028     C*
028600141028     C* Reimposto i valori di default
028700141028     C                   EXSR      DEFCAM
028800141028     C*
028900141028     C                   ENDSR
029000141028     C*----------------------------------------------------*
029100141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029200141028     C*----------------------------------------------------*
029300141028     C     DEFCAM        BEGSR
029400141028     C*
029500141028     C                   CLEAR                   EDIVAB00
029600141028     C                   CLEAR                   EDIVAT00
029700141028     C*
029800141028     C* Imposto i valori bolla di default
029900141028     C                   EVAL      VABCTR = 000
030000141028     C                   EVAL      VABCBO = '1'
030100141104     C                   EVAL      VABTSP = 'C'
030200141028     C*
030300141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030400141028     C* e delimitatore testo.
030500141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030600141028     C*
030700141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030800141028     C                   EVAL      CharSOS = CharNUM
030900141028     C*
031000141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
031100141028     C                   SETOFF                                       53
031200141028     C                   SELECT
031300141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
031400141028     C                   SETON                                        53
031500141028     C                   ENDSL
031600141028     C*
031700141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031800141028     C                   EVAL      posDaDft = 1
031900141028     C                   EVAL      posADft  = 0
032000141028     C                   EVAL      wGiroDft = 0
032100141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032200141028     C                             posDaDft > 0
032300141028     C*
032400141028     C* Gestisco il 1° giro
032500141028     C                   IF        wGiroDft = 0
032600141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
032700141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032800141028     C* Incremento il contatore dei "giri"
032900141028     C                   EVAL      wGiroDft = 1
033000141028     C                   ELSE
033100141028     C                   EVAL      posDaDft = posADft
033200141028     C                   ENDIF
033300141028     C* Eseguo lo scan x trovare la fine del campo corrente
033400141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033500141028     C*
033600141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
033700141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
033800141028     C                   IF        posDaDft > 0 AND
033900141028     C                             posADft  > 0
034000141028     C* NCL
034100141028     C                   IF        %subst(
034200141028     C                             %subst(vlrppt:posDaDft+1:
034300141028     C                             posADft-posDaDft-1):1:3)
034400141028     C                             = 'NCL'
034500141028     C                   EVAL      PiStr=%trim(%subst(
034600141028     C                             %subst(vlrppt:posDaDft+1:
034700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034800141028     C                   EXSR      CHKNUM
034900141028     C                   IF        PiInt=*on
035000141028     C                   Z-ADD     PiVal         VABNCL
035100141028     C                   ENDIF
035200141028     C                   ENDIF
035300141028     C* CCM
035400141028     C                   IF        %subst(
035500141028     C                             %subst(vlrppt:posDaDft+1:
035600141028     C                             posADft-posDaDft-1):1:3)
035700141028     C                             = 'CCM'
035800141028     C                   EVAL      PiStr=%trim(%subst(
035900141028     C                             %subst(vlrppt:posDaDft+1:
036000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036100141028     C                   EXSR      CHKNUM
036200141028     C                   IF        PiInt=*on
036300141028     C                   Z-ADD     PiVal         VABCCM
036400141028     C                   ENDIF
036500141028     C                   ENDIF
036600141028     C* LNP
036700141028     C                   IF        %subst(
036800141028     C                             %subst(vlrppt:posDaDft+1:
036900141028     C                             posADft-posDaDft-1):1:3)
037000141028     C                             = 'LNP'
037100141028     C                   EVAL      PiStr=%trim(%subst(
037200141028     C                             %subst(vlrppt:posDaDft+1:
037300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037400141028     C                   EXSR      CHKNUM
037500141028     C                   IF        PiInt=*on
037600141028     C                   Z-ADD     PiVal         VABLNP
037700141028     C                   ENDIF
037800141028     C                   ENDIF
037900141028     C* NRS
038000141028     C                   IF        %subst(
038100141028     C                             %subst(vlrppt:posDaDft+1:
038200141028     C                             posADft-posDaDft-1):1:3)
038300141028     C                             = 'NRS'
038400141028     C                   EVAL      PiStr=%trim(%subst(
038500141028     C                             %subst(vlrppt:posDaDft+1:
038600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038700141028     C                   EXSR      CHKNUM
038800141028     C                   IF        PiInt=*on
038900141028     C                   Z-ADD     PiVal         VABNRS
039000141028     C                   ENDIF
039100141028     C                   ENDIF
039200141028     C* CTR
039300141028     C                   IF        %subst(
039400141028     C                             %subst(vlrppt:posDaDft+1:
039500141028     C                             posADft-posDaDft-1):1:3)
039600141028     C                             = 'CTR'
039700141028     C                   EVAL      PiStr=%trim(%subst(
039800141028     C                             %subst(vlrppt:posDaDft+1:
039900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040000141028     C                   EXSR      CHKNUM
040100141028     C                   IF        PiInt=*on
040200141028     C                   Z-ADD     PiVal         VABCTR
040300141028     C                   ENDIF
040400141028     C                   ENDIF
040500141028     C* PKB
040600141028     C                   IF        %subst(
040700141028     C                             %subst(vlrppt:posDaDft+1:
040800141028     C                             posADft-posDaDft-1):1:3)
040900141028     C                             = 'PKB'
041000141028     C                   EVAL      PiStr=%trim(%subst(
041100141028     C                             %subst(vlrppt:posDaDft+1:
041200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041300141028     C                   EXSR      CHKNUM
041400141028     C                   IF        PiNum=*on
041500141028     C                   Z-ADD     PiVal         VABPKB
041600141028     C                   ENDIF
041700141028     C                   ENDIF
041800141028     C* VLB
041900141028     C                   IF        %subst(
042000141028     C                             %subst(vlrppt:posDaDft+1:
042100141028     C                             posADft-posDaDft-1):1:3)
042200141028     C                             = 'VLB'
042300141028     C                   EVAL      PiStr=%trim(%subst(
042400141028     C                             %subst(vlrppt:posDaDft+1:
042500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042600141028     C                   EXSR      CHKNUM
042700141028     C                   IF        PiNum=*on
042800141028     C                   Z-ADD     PiVal         VABVLB
042900141028     C                   ENDIF
043000141028     C                   ENDIF
043100141028     C* QFT
043200141028     C                   IF        %subst(
043300141028     C                             %subst(vlrppt:posDaDft+1:
043400141028     C                             posADft-posDaDft-1):1:3)
043500141028     C                             = 'QFT'
043600141028     C                   EVAL      PiStr=%trim(%subst(
043700141028     C                             %subst(vlrppt:posDaDft+1:
043800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043900141028     C                   EXSR      CHKNUM
044000141028     C                   IF        PiNum=*on
044100141028     C                   Z-ADD     PiVal         VABQFT
044200141028     C                   ENDIF
044300141028     C                   ENDIF
044400141028     C* CBO
044500141028     C                   IF        %subst(
044600141028     C                             %subst(vlrppt:posDaDft+1:
044700141028     C                             posADft-posDaDft-1):1:3)
044800141028     C                             = 'CBO'
044900141028     C                   EVAL      VABCBO=%trim(%subst(
045000141028     C                             %subst(vlrppt:posDaDft+1:
045100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045200141028     C                   ENDIF
045300141028     C* TSP
045400141028     C                   IF        %subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):1:3)
045700141028     C                             = 'TSP'
045800141028     C                   EVAL      VABTSP=%trim(%subst(
045900141028     C                             %subst(vlrppt:posDaDft+1:
046000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100141028     C                   ENDIF
046200141028     C* VAS
046300141028     C                   IF        %subst(
046400141028     C                             %subst(vlrppt:posDaDft+1:
046500141028     C                             posADft-posDaDft-1):1:3)
046600141028     C                             = 'VAS'
046700141028     C                   EVAL      VABVAS=%trim(%subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000141028     C                   ENDIF
047100141028     C* VCA
047200141028     C                   IF        %subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):1:3)
047500141028     C                             = 'VCA'
047600141028     C                   EVAL      VABVCA=%trim(%subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900141028     C                   ENDIF
048000141028     C* TIC
048100141028     C                   IF        %subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):1:3)
048400141028     C                             = 'TIC'
048500141028     C                   EVAL      VABTIC=%trim(%subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800141028     C                   ENDIF
048900141028     C* GCA
049000141028     C                   IF        %subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):1:3)
049300141028     C                             = 'GCA'
049400141028     C                   EVAL      VABGCA=%trim(%subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700141028     C                   ENDIF
049800141028     C* CTM
049900141028     C                   IF        %subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):1:3)
050200141028     C                             = 'CTM'
050300141028     C                   EVAL      VABCTM=%trim(%subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600141028     C                   ENDIF
050700141028     C* FFD
050800141028     C                   IF        %subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):1:3)
051100141028     C                             = 'FFD'
051200141028     C                   EVAL      VABFFD=%trim(%subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500141028     C                   ENDIF
051600141028     C* VAD
051700141028     C                   IF        %subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):1:3)
052000141028     C                             = 'VAD'
052100141028     C                   EVAL      VABVAD=%trim(%subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400141028     C                   ENDIF
052500141028     C* GMA
052600141028     C                   IF        %subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):1:3)
052900141028     C                             = 'GMA'
053000141028     C                   EVAL      VABGMA=%trim(%subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300141028     C                   ENDIF
053400141028     C* GGA
053500141028     C                   IF        %subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):1:3)
053800141028     C                             = 'GGA'
053900141028     C                   EVAL      VABGGA=%trim(%subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200141028     C                   ENDIF
054300141028     C* GVA
054400141028     C                   IF        %subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):1:3)
054700141028     C                             = 'GVA'
054800141028     C                   EVAL      VABGVA=%trim(%subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100141028     C                   ENDIF
055200141028     C* TC1
055300141028     C                   IF        %subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):1:3)
055600141028     C                             = 'TC1'
055700141028     C                   EVAL      VABTC1=%trim(%subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000141028     C                   ENDIF
056100141028     C* TC2
056200141028     C                   IF        %subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):1:3)
056500141028     C                             = 'TC2'
056600141028     C                   EVAL      VABTC2=%trim(%subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900141028     C                   ENDIF
057000141028     C* VATTRC
057100141028     C                   IF        %subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):1:3)
057400141028     C                             = 'TRC'
057500141028     C                   EVAL      VATTRC=%trim(%subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800141028     C                   ENDIF
057900141028     C* ...
058000141028     C                   ENDIF
058100141028     C                   ENDDO
058200141028     C*
058300141028     C                   ENDSR
058400000801     C*----------------------------------------------------*
058500140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
058600000801     C*----------------------------------------------------*
058700040910     C     IMPVABVAT     BEGSR
058800040910     C*
058900040910     C* Traduzione relativa ai tipi record del file del cliente
059000040910     C*
059100071210     C*
059200071210     C***
059300141028     C* ...tipo record 'UNB' (info CMR)
059400141028     C                   EVAL      wTag = 'UNB'
059500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
059600141028     C                   MOVEL     *blanks       savCMR           35
059700141028     C                   EVAL      InStringa =
059800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
059900141028     C                   EVAL      InSepara = '+'
060000141028     C                   EXSR      SR_SPLIT
060100141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
060200140922     C                                      %char(datcor) + ' ' + %char(oracor)
060300130620     C                   ENDIF
060400141028     C*
060500141028     C***
060600141028     C* ...tipo record 'UNH' (rottuta di codice spedizione)
060700141028     C                   EVAL      wTag = 'UNH'
060800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
060900141028     C                   SETOFF                                       31
061000141028     C                   eval      vinflg = '1'
061100141028     C* ......inizializzazioni iniziali e formati record file Bartolini
061200141028     C                   EXSR      INZVAR
061300141028     C* ......valorizzazione campi fissi
061400141028     C                   MOVEL     datcor        VABAAS
061500141028     C                   MOVE      datcor        VABMGS
061600141028     C                   MOVE(P)   vlrpoi        VABFGS
061700141028     C                   EVAL      VABCMR = savCMR
061800141028     C                   EVAL      VABDCM = datcor
061900141028     C                   EVAL      VABDTS = datcor
062000141028     C                   EVAL      VABCNT = 1
062100141028     C* ......NSP => Stacco un numeratore da AZNUM
062200141028     C                   clear                   TRUL33DS
062300141028     C                   eval      I33OPE = *zeros
062400141028     C                   eval      I33CNU = 302
062500141028     C                   eval      I33NUM = 1
062600141028     C                   movel     TRUL33DS      KPJBU
062700141028     C                   call      'TRUL33R'
062800141028     C                   parm                    KPJBA
062900141028     C                   movel     KPJBU         TRUL33DS
063000141028     C                   if        O33ERR = *zeros
063100141028     C                   z-add     O33NRF        VABNSP
063200141028     C                   else
063300141028     C                   Z-ADD     1             wRecErr
063400141028     C                   SETON                                        31
063500141028     C                   EVAL      vinmsg = %trimr(vinmsg)
063600141028     C                             + ' ' + 'VABNSP VATNSP'
063700141028     C                   endif
063800141028     C                   ENDIF
063900141028     C*
064000141028     C***
064100141028     C* ...tipo record 'RFF+DQ' (riferimento spedizione)
064200141028     C                   EVAL      wTag = 'RFF+DQ'
064300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064400141028     C                   EVAL      VABRMA =
064500141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
064600141028     C                   ENDIF
064700151102     C*
064800151102     C***
064900151102     C* ...tipo record 'DTM+69' (data consegna richiesta)
065000151102     C                   EVAL      wTag = 'DTM+69'
065100151102     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
065200151102     C                   EVAL      PiStr=%subst(
065300151102     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
065400151102     C                   EXSR      CHKNUM
065500151102     C                   IF        PiInt=*on
065600151102     C                   Z-ADD     PiVal         VABDCR
065700151102     C                   ELSE
065800151102     C                   Z-ADD     1             wRecErr
065900151102     C                   ENDIF
066000151102     C                   ENDIF
066100151124     C*
066200151124     C***
066300151124     C* ...tipo record 'FTX+DEL' (riferimenti spedizione)
066400151124     C                   EVAL      wTag = 'FTX+DEL'
066500151124     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066600160229     C                   EVAL      InStringa =
066700160229     C                                %subst(wvindta:%len(%trim(wTag))+2)
066800160229     C                   EVAL      InSepara = '+'
066900160229     C                   EXSR      SR_SPLIT
067000151124     C* RMA
067100160229     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(3))
067200151124     C* RMN
067300160229     C                   EVAL      PiStr= VABRMA
067400151124     C                   EXSR      CHKNUM
067500151124     C                   IF        PiInt=*on
067600151124     C                   Z-ADD     PiVal         VABRMN
067700151124     C                   ELSE
067800151124     C                   Z-ADD     1             wRecErr
067900151124     C                   ENDIF
068000151124     C                   ENDIF
068100141028     C*
068200141028     C***
068300141028     C* ...tipo record 'NAD+CN' (destinatario)
068400141028     C                   EVAL      wTag = 'NAD+CN'
068500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068600141028     C                   EVAL      InStringa =
068700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
068800141028     C                   EVAL      InSepara = '+'
068900141028     C                   EXSR      SR_SPLIT
069000141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
069100141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
069200141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
069300141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
069400141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
069500141028     C                   IF        VABNZD = 'I'   OR
069600141028     C                             VABNZD = 'IT'  OR
069700141028     C                             VABNZD = 'ITA'
069800141028     C                   EVAL      VABNZD = *blanks
069900141028     C                   ENDIF
070000141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
070100141028     C                   EVAL      InSepara = ':'
070200141028     C                   EXSR      SR_SPLIT
070300141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
070400141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
070500141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
070600141028     C                                      %trim(SkSplitCSV_5(4))+
070700141028     C                                      %trim(SkSplitCSV_5(5))
070800141028     C                   ENDIF
070900141104     C*
071000141104     C***
071100141104     C* ...tipo record 'NAD+UC' (destinatario)
071200141104     C                   EVAL      wTag = 'NAD+UC'
071300141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071400141104     C                   EVAL      InStringa =
071500141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
071600141104     C                   EVAL      InSepara = '+'
071700141104     C                   EXSR      SR_SPLIT
071800141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
071900141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
072000141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
072100141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
072200141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
072300141104     C                   IF        VABNZD = 'I'   OR
072400141104     C                             VABNZD = 'IT'  OR
072500141104     C                             VABNZD = 'ITA'
072600141104     C                   EVAL      VABNZD = *blanks
072700141104     C                   ENDIF
072800141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
072900141104     C                   EVAL      InSepara = ':'
073000141104     C                   EXSR      SR_SPLIT
073100141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
073200141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
073300141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
073400141104     C                                      %trim(SkSplitCSV_5(4))+
073500141104     C                                      %trim(SkSplitCSV_5(5))
073600141104     C                   ENDIF
073700141028     C*
073800141028     C***
073900141028     C* ...tipo record 'GID' (colli)
074000141028     C                   EVAL      wTag = 'GID'
074100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074200141028     C                   EVAL      InStringa =
074300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
074400141028     C                   EVAL      InSepara = '+'
074500141028     C                   EXSR      SR_SPLIT
074600160201     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
074700160201     C                   EVAL      InSepara = ':'
074800160201     C                   EXSR      SR_SPLIT
074900160201     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
075000141028     C                   EXSR      CHKNUM
075100141028     C                   IF        PiInt=*on
075200141028     C                   Z-ADD     PiVal         VABNCL
075300141028     C                   ELSE
075400141028     C                   Z-ADD     1             wRecErr
075500141028     C                   ENDIF
075600141028     C                   ENDIF
075700141028     C*
075800141028     C***
075900141028     C* ...tipo record 'FTX+AAA' (natura merce)
076000141028     C                   EVAL      wTag = 'FTX+AAA'
076100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076200141028     C                   EVAL      InStringa =
076300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
076400141028     C                   EVAL      InSepara = '+'
076500141028     C                   EXSR      SR_SPLIT
076600141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
076700141028     C                   ENDIF
076800170123     C*
076900170123     C***
077000170123     C* ...tipo record 'FTX+DIN' (note di consegna)
077100170123     C                   EVAL      wTag = 'FTX+DIN'
077200170123     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077300170123     C                   EVAL      InStringa =
077400170123     C                                %subst(wvindta:%len(%trim(wTag))+2)
077500170123     C                   EVAL      InSepara = '+'
077600170123     C                   EXSR      SR_SPLIT
077700170123     C                   SELECT
077800170123     C                   WHEN      VABNOT = *blanks AND VABNT2 = *blanks
077900170123     C                   EVAL      VABNOT = %subst(SkSplitCSV_5(3):1:35)
078000170123     C                   EVAL      VABNT2 = %subst(SkSplitCSV_5(3):36:35)
078100170123     C                   WHEN      VABNOT = *blanks
078200170123     C                   EVAL      VABNOT = SkSplitCSV_5(3)
078300170123     C                   WHEN      VABNT2 = *blanks
078400170123     C                   EVAL      VABNT2 = SkSplitCSV_5(3)
078500170123     C                   ENDSL
078600170123     C                   ENDIF
078700141028     C*
078800141028     C***
078900141028     C* ...tipo record 'MEA+WT+AAE' (peso spedizione)
079000141028     C                   EVAL      wTag = 'MEA+WT+AAE'
079100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079200141028     C                   EVAL      InStringa =
079300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079400141028     C                   EVAL      InSepara = ':'
079500141028     C                   EXSR      SR_SPLIT
079600141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
079700141028     C                   EXSR      CHKNUM
079800141028     C                   IF        PiNum=*on
079900150924     C                   ADD       PiVal         VABPKB
080000141028     C                   ELSE
080100141028     C                   Z-ADD     1             wRecErr
080200141028     C                   ENDIF
080300141028     C                   ENDIF
080400141028     C*
080500141028     C***
080600141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
080700141028     C                   EVAL      wTag = 'MEA+VOL'
080800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080900141028     C                   EVAL      InStringa =
081000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
081100141028     C                   EVAL      InSepara = ':'
081200141028     C                   EXSR      SR_SPLIT
081300141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
081400141028     C                   EXSR      CHKNUM
081500141028     C                   IF        PiNum=*on
081600150924     C                   ADD       PiVal         VABVLB
081700141028     C                   ELSE
081800141028     C                   Z-ADD     1             wRecErr
081900141028     C                   ENDIF
082000141028     C                   ENDIF
082100141028     C*
082200141028     C***
082300141028     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
082400141028     C                   EVAL      wTag = 'GIN+BJ'
082500151218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082600151218     C                   EVAL      InStringa =
082700151218     C                                %subst(wvindta:%len(%trim(wTag))+2)
082800151218     C                   EVAL      InSepara = '+'
082900151218     C                   EXSR      SR_SPLIT
083000151218     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
083100151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
083200151218     C                   EVAL      VATTRC = 'E'
083300151218     C                   EXSR      WRIVAT
083400151218     C                   ENDIF
083500151218     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
083600151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
083700151218     C                   EVAL      VATTRC = 'E'
083800151218     C                   EXSR      WRIVAT
083900151218     C                   ENDIF
084000151218     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
084100151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
084200151218     C                   EVAL      VATTRC = 'E'
084300151218     C                   EXSR      WRIVAT
084400151218     C                   ENDIF
084500151218     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
084600151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
084700151218     C                   EVAL      VATTRC = 'E'
084800151218     C                   EXSR      WRIVAT
084900151218     C                   ENDIF
085000151218     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
085100151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
085200151218     C                   EVAL      VATTRC = 'E'
085300151218     C                   EXSR      WRIVAT
085400151218     C                   ENDIF
085500151218     C                   ENDIF
085600141028     C*
085700141028     C***
085800141028     C* ...tipo record '???' (PDF per packing-list)
085900141028     C                   EVAL      wTag = '???PDF'
086000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086100141028     C*
086200141028     C* ......VATNOT_P
086300141028     C                   if        *in53
086400141028     C*
086500141028     C* Solo se valorizzato RMN
086600141028     C                   if        VABRMA <> *blanks
086700141028     C                   eval      pIn_MaskPDF =
086800141028     C                                     %trim(%subst(wvindta:1292:100))
086900141028     C                   call      'TIS7P2SER'
087000141028     C                   parm      'W'           pIn_Opz           1
087100141028     C                   parm                    tivlrds
087200141028     C                   parm                    EDIVABDS
087300141028     C                   parm      '10'          pIn_CdIdAz        2
087400141028     C                   parm                    pIn_MaskPDF      50
087500141028     C                   parm      *blanks       pOut_Esito        1
087600141028     C                   endif
087700141028     C                   endif
087800141028     C*
087900141028     C                   ENDIF
088000141028     C*
088100141028     C***
088200141028     C* ...tipo record 'UNT' (fine bolla)
088300141028     C                   EVAL      wTag = 'UNT'
088400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088500141028     C  N31              ADD       1             §CTROKVB
088600141028     C  N31              exsr      CHKWRI
088700140117     C  N31              WRITE     EDIVAB00
088800100722     C                   endif
088900010202     C*
089000000801     C* Ebbene...
089100000801     C                   ADD       1             §CTRMO
089200141028     C                   IF        wRecErr <> *zeros
089300000801     C                   ADD       1             §CTRNO
089400000801     C                   EVAL      vinflg = '2'
089500000801     C                   ENDIF
089600000801     C*
089700000801     C                   ENDSR
089800010202     C*----------------------------------------------------*
089900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
090000010202     C*----------------------------------------------------*
090100020305     C     PREVAT        BEGSR
090200010202     C*
090300140117     C* Compongo il nome del membro da dare al EDIVATWR
090400010202     C                   eval      parmbr = vlrhdl
090500010202     C                   movel     'M'           parmbr
090600060113     C                   eval      parccm = vlrksc
090700010202     C                   eval      paropz = '1'
090800010202     C* Effettuo la chiamata al CLLE preposto
090900140117     C                   call(e)   'TITVEVTC'
091000010202     C                   parm                    parccm
091100010202     C                   parm                    parmbr
091200010202     C                   parm                    paropz
091300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
091400010202     C                   if        %error
091500010202     C                   movel     '1'           chkcall
091600010202     C                   else
091700010202     C                   movel     '0'           chkcall
091800010202     C                   endif
091900010202     C*
092000010202     C                   ENDSR
092100141028     C*----------------------------------------------------*
092200141028     C*  CONTROLLO NUMERICITA' CAMPI
092300141028     C*----------------------------------------------------*
092400141028     C     CHKNUM        BEGSR
092500141028     C*
092600141028     C                   IF        PiDecChr = *blanks
092700141028     C                   EVAL      PiDecChr = CharNUM
092800141028     C                   ENDIF
092900141028     C*
093000141028     C                   callp     UBISNUM_Check(PiStr
093100141028     C                                          :PiDecChr
093200141028     C                                          :PiVal
093300141028     C                                          :PiNum
093400141028     C                                          :PiInt)
093500141028     C*
093600141028     C                   ENDSR
093700141028     C***
093800141028
093900141028
094000141028     C*----------------------------------------------------*
094100141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
094200141028     C*----------------------------------------------------*
094300141028     C     SR_SPLIT      BEGSR
094400141028     C*
094500141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
094600141028     C                   CALL      'XSPLIT2'
094700141028     C                   PARM                    InStringa
094800141028     C                   PARM                    InSepara
094900141028     C                   PARM      '5'           InTypeOut
095000141028     C                   PARM      ''            wSkParam
095100141028     C                   PARM                    OutErrore
095200141028     C                   PARM                    OutMsg
095300141028     C                   MOVEA     wSkParam      SkSplitCSV_5
095400141028     C*
095500141028     C                   ENDSR
095600141028
095700141028
095800141028     C*----------------------------------------------------*
095900141028     C*  SCRITTURA BUFFER EDIVAT
096000141028     C*----------------------------------------------------*
096100141028     C     WRIVAT        BEGSR
096200141028     C*
096300141028     C                   IF        VATNOT <> *blanks
096400141028     C                   EVAL      VATFGS = VABFGS
096500141028     C                   EVAL      VATCCM = VABCCM
096600141028     C                   EVAL      VATLNP = VABLNP
096700141028     C                   EVAL      VATAAS = VABAAS
096800141028     C                   EVAL      VATNRS = VABNRS
096900141028     C                   EVAL      VATNSP = VABNSP
097000141028     C                   EVAL      VATCMR = VABCMR
097100141028     C                   EVAL      VATCNT = VABCNT
097200141028     C                   ADD       1             §CTROKVT
097300141028     C                   WRITE     EDIVAT00
097400141028     C                   ENDIF
097500141028     C*
097600141028     C                   ENDSR
097700000801
097800011113
097900011113     C*----------------------------------------------------*
098000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
098100011113     C*----------------------------------------------------*
098200011113     C     CHKIMPDIV     BEGSR
098300011113     C*
098400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
098500011113     C                   Z-ADD     *zeros        wrkDec            9 9
098600011113     C*
098700011113     C* Come prima cosa effettuo considerazioni sulla divisa
098800011113     C                   IF        vabIAS > *zeros
098900011113     C                   IF        vabVAS <> 'EUR'
099000011113     C                   EVAL      vabVAS =  'ITL'
099100011113     C                   ENDIF
099200011113     C                   ENDIF
099300011113     C*
099400011113     C                   IF        vabCAS > *zeros
099500011113     C                   IF        vabVCA <> 'EUR'
099600011113     C                   EVAL      vabVCA =  'ITL'
099700011113     C                   ENDIF
099800011113     C                   ENDIF
099900011113     C*
100000011113     C                   IF        vabVMD > *zeros
100100020305     C                   IF        vabVAD <> 'EUR'
100200011113     C                   EVAL      vabVAD =  'ITL'
100300011113     C                   ENDIF
100400011113     C                   ENDIF
100500011113     C*
100600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
100700011113     C                   Z-ADD     vabIAS        wrkDec
100800011113     C                   IF        wrkDec > *zeros
100900011113     C                   IF        vabVAS = 'ITL'
101000011113     C                   EVAL      vabIAS = *zeros
101100011113     C                   ENDIF
101200011113     C                   ENDIF
101300011113     C*
101400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
101500011113     C                   Z-ADD     vabCAS        wrkDec
101600011113     C                   IF        wrkDec > *zeros
101700011113     C                   IF        vabVCA = 'ITL'
101800011113     C                   EVAL      vabCAS = *zeros
101900011113     C                   ENDIF
102000011113     C                   ENDIF
102100011113     C*
102200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
102300011113     C                   Z-ADD     vabVMD        wrkDec
102400011113     C                   IF        wrkDec > *zeros
102500011113     C                   IF        vabVAD = 'ITL'
102600011113     C                   EVAL      vabVMD = *zeros
102700011113     C                   ENDIF
102800011113     C                   ENDIF
102900011113     C*
103000011113     C                   ENDSR
103100011113     C***
103200011113
103300011113
103400000801
103500000801
103600990920      /TITLE Invio dei dati al punto operativo.
103700010202     C     invio         BEGSR
103800990920     C*
103900140117     C* 1° invio EDIVAT
104000010201     C                   reset                   dscmz
104100010201     C                   move      vlrpoi        cmzdst
104200140117     C                   eval      cmzfld = 'EDIVATWR'
104300010201     C                   eval      cmzmbd = vlrhdl
104400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
104500021009     C***                if        prmfir = *blanks
104600140117     C                   eval      cmzfla = 'EDIVAT0F'
104700140117     C                   eval      cmzmba = 'EDIVAT0F'
104800021009     C***                else
104900021009     C***                eval      cmzfla = prmfir
105000021009     C***                eval      cmzmba = prmfir
105100021009     C***                endif
105200010201     C                   eval      cmznrr = *zeros
105300020305     C                   move      §ctrokvt      cmznrr
105400021018     C                   eval      cmzlba = vlrfl1
105500010201     C                   call(e)   'TIS711C'
105600010201     C                   parm                    dscmz
105700010201     C                   parm      *blanks       esito
105800010205     C                   if        %error
105900010205     C                             or cmzerr = '1'
106000010205     C                             or esito  = '1'
106100010205     C                   eval      wrkesito = '3'
106200010205     C                   else
106300010201     C*
106400140117     C* 2° invio EDIVAB
106500010201     C                   reset                   dscmz
106600010201     C                   move      vlrpoi        cmzdst
106700010201     C                   eval      cmzfld = vlrfou
106800010201     C                   eval      cmzmbd = vlrhdl
106900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
107000021009     C***                if        prmfir = *blanks
107100140117     C                   eval      cmzfla = 'EDIVAB0F'
107200140117     C                   eval      cmzmba = 'EDIVAB0F'
107300021009     C***                else
107400021009     C***                eval      cmzfla = prmfir
107500021009     C***                eval      cmzmba = prmfir
107600021009     C***                endif
107700010201     C                   eval      cmznrr = *zeros
107800010201     C                   move      §ctrokvb      cmznrr
107900021018     C                   eval      cmzlba = vlrfl1
108000010201     C                   call(e)   'TIS711C'
108100010201     C                   parm                    dscmz
108200010201     C                   parm      *blanks       esito
108300010201     C                   if        %error
108400010201     C                             or cmzerr = '1'
108500010201     C                             or esito  = '1'
108600010201     C                   eval      wrkesito = '3'
108700010201     C                   endif
108800010205     C                   endif
108900990920     C*
109000000613     C                   ENDSR
109100000613     C***
109200070411
109300141028
109400070411     C     *pssr         BEGSR
109500070411     C*
109600070411     C                   if        %open(tivin00r)
109700070411     C                   close     tivin00r
109800070411     C                   endif
109900140117     C                   if        %open(edivabwr)
110000140117     C                   close     edivabwr
110100070411     C                   endif
110200140117     C                   if        %open(edivatwr)
110300140117     C                   close     edivatwr
110400070411     C                   endif
110500070411     C*
110600070411     C* Effettuo la chiamata al CLLE preposto
110700140117     C                   call(e)   'TITVEVTC'
110800070411     C                   parm                    parccm
110900070411     C                   parm                    parmbr
111000070411     C                   parm      '2'           paropz
111100070411     C*
111200070411     C                   eval      wrkesito = '2'
111300070411     C*
111400070411     C                   seton                                        LR
111500070411     C*
111600070411     C                   ENDSR     '*CANCL'
111700070411     C***
111800070411
111900990910
112000000613     C     *inzsr        BEGSR
112100990910     C*
112200990910     C     *entry        plist
112300990920     C                   parm                    tivlrds
112400990921     C                   parm      wrkesito      esito
112500000724     C                   parm                    prmlit
112600000710     C                   parm                    prmfir
112700000613     C*
112800000830     C* CALCOLA LA DATA CORRENTE
112900140117     C                   time                    wn14             14 0
113000140117     C                   movel     wn14          oracor            6 0          *ORA
113100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
113200100722     C                   eval      datcor = %dec(%date() : *ISO)
113300000830     C*
113400000613     C                   ENDSR
113500000613     C***
