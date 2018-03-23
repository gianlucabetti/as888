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
003800170704     D wVABCTE         s                   LIKE(VABCTR) INZ
003900141028
004000141028     D*------------------
004100141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004200141028     D*------------------
004300141028     D CharNUM         S              1
004400141028     D CharSOS         S              1
004500141028     D posDaDft        S              3  0 INZ(*zeros)
004600141028     D posADft         S              3  0 INZ(*zeros)
004700141028     D wGiroDft        s              1  0 INZ(*zeros)
004800141028
004900141028
005000141028     D*------------------
005100141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005200141028     D*------------------
005300141028     D InStringa       S          65535A   VARYING                              (stringa input)
005400141028     D InSepara        S             10A                                        (separatore)
005500141028     D InTypeOut       S              1A                                        (tipo output)
005600141028     D wSkParam        S          65535A                                        (output)
005700141028     D OutErrore       S              1A                                        (flag errore)
005800141028     D OutMsg          S             80A                                        (messaggio errore)
005900141028     D SkSplitCSV_5    S            256    DIM(256)
006000141028
006100000830
006200041025     D*------------------
006300041025     D* DS REPERIMENTO NUMERATORE
006400041025     D*------------------
006500041025     D trul33ds      e ds                  inz
006600041025     D*------------------
006700041025     D* DS ARCHITETTURA
006800041025     D*------------------
006900041025     D kpjba         e ds                  inz
007000041025     D*------------------
007100990908
007200141028
007300141028     D*------------------
007400141028     D* LINKING A DEFINIZIONI ESTERNE
007500141028     D*------------------
007600141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800150609
007900141028
008000150609     D*-------------------
008100150609     D* COSTANTI
008200150609     D*-------------------
008300150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500141028
008600010201
008700010201
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100150526     C                   setoff                                       90
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400070719     C*
009500070719     C* Esegue lancio TISI95R solo x chiusura
009600070719     C                   CLEAR                   TISI95DS
009700070719     C                   EVAL      I95TLA = 'C'
009800070719     C                   CALL      'TISI95R'
009900070719     C                   PARM                    TISI95DS
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200140922     C                   call(e)   'TITVEVTC'
010300140922     C                   parm                    parccm
010400140922     C                   parm                    parmbr
010500140922     C                   parm      '2'           paropz
010600170731     C*
010700170731     C                   if        *in53
010800170731     C                   call      'TIS7P2SER'
010900170731     C                   parm      'C'           pIn_Opz           1
011000170731     C                   parm                    tivlrds
011100170731     C                   parm                    EDIVABDS
011200170731     C                   parm      *blanks       pIn_CdIdAz        2
011300170731     C                   parm      *blanks       pIn_MaskPDF      50
011400170731     C                   parm      *blanks       pOut_Esito        1
011500170731     C                   endif
011600000616     C*
011700010201     C                   seton                                        LR
011800990908
011900000801
012000910830     C*--------------------------------------------------------
012100140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012200910830     C*--------------------------------------------------------
012300040526     C     RWFILE        BEGSR
012400990910     C*
012500990914     C                   if        not %open(tivin00r)
012600990908     C                   open      tivin00r
012700990914     C                   endif
012800140117     C                   if        not %open(edivabwr)
012900140117     C                   open      edivabwr
013000990914     C                   endif
013100141028     C*
013200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
013300020305     C                   exsr      prevat
013400010201     C*
013500010202     C                   if        chkcall = '0'
013600010202     C*
013700140117     C                   if        not %open(edivatwr)
013800140117     C                   open      edivatwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   clear                   §CTROKVB          5 0
014200020305     C                   clear                   §CTROKVT          5 0
014300000801     C                   clear                   §CTRMO            5 0
014400000801     C                   clear                   §CTRNO            5 0
014500141028     C*
014600141028     C* Inizializzazioni fuori ciclo
014700141028     C                   EXSR      INZVAR
014800140117     C*
014900140902     C                   if        *in53
015000140117     C                   call      'TIS7P2SER'
015100140117     C                   parm      'O'           pIn_Opz           1
015200140117     C                   parm                    tivlrds
015300140117     C                   parm                    EDIVABDS
015400140117     C                   parm      *blanks       pIn_CdIdAz        2
015500140117     C                   parm      *blanks       pIn_MaskPDF      50
015600140117     C                   parm      *blanks       pOut_Esito        1
015700140902     C                   endif
015800130708     C*
015900921023     C                   DO        *HIVAL
016000990913     C*
016100990915     C                   READ      tivin00r                               70
016200040910     C                   if        vindta > *blanks
016300000613     C                   add       1             rrnum
016400000801     C*
016500000801     C                   if        *in70 = *off
016600000801     C                             and
016700000801     C                             (vinflg = *blanks
016800000801     C                              or vinflg = '0'
016900000801     C                              or vinflg = '2')
017000000801     C*
017100000801     C                   clear                   vinmsg
017200141028     C*
017300141028     C* "normalizzo" il dato di input
017400141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
017500141028     C                                             %len(%trim(vindta))-1))
017600160209     C*
017700160209     C* Elimino le "duple di controllo EDIFACT"
017800160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017900160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018000160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018100160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018200160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
018300040910     C*
018400040910     C* Eseguo routine d traduzione
018500040910     C                   exsr      impvabvat
018600040802     C*
018700010305     C                   endif
018800000905     C*
018900000905     C                   else
019000000905     C                   eval      vinflg = '1'
019100000905     C                   endif
019200000905     C*
019300000905     C  N70              update    tivin000
019400000905     C*
019500991022     C  N70              ENDdo
019600100722     C*
019700100722     C* Scarico i buffer testata ancora "in canna"
019800140922     C*
019900141028     C***  N31              ADD       1             §CTROKVB
020000141028     C***  N31              exsr      CHKWRI
020100141028     C***  N31              WRITE     EDIVAB00
020200010202     C*
020300010202     C                   endif
020400990910
020500990910     C* Se non ci sono record con errori ...
020600000710     C                   if        §ctrno = 0
020700990910     C* ... restituisco esito OK.
020800990921     C                   eval      wrkesito = '0'
020900990910     C                   else
021000010201     C                   if        §ctrokvb > 0
021100990921     C                   eval      wrkesito = '1'
021200000710     C                   else
021300000710     C                   eval      wrkesito = '2'
021400990910     C                   endif
021500000710     C                   endif
021600990910     C*
021700990914     C                   if        %open(tivin00r)
021800990908     C                   close     tivin00r
021900990914     C                   endif
022000140117     C                   if        %open(edivabwr)
022100140117     C                   close     edivabwr
022200990914     C                   endif
022300140117     C                   if        %open(edivatwr)
022400140117     C                   close     edivatwr
022500010201     C                   endif
022600990910     C*
022700010201     C                   if        §ctrokvb > 0
022800000724     C                             and vlrpoi <> *zeros
022900010202     C                   exsr      invio
023000990920     C                   endif
023100990920     C*
023200910830     C                   ENDSR
023300000613     C***
023400140922
023500140922     C*----------------------------------------------------*
023600140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
023700140922     C*----------------------------------------------------*
023800140922     C     CHKWRI        BEGSR
023900140922     C*
024000141028     C                   if        VABRMN = *zeros
024100141028     C                   eval      VABRMN = VABNSP
024200141028     C                   endif
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
027200150527     C*
027300150527     C* Scompongo le note
027400150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
027500150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
027600140922     C*
027700140922     C                   ENDSR
027800141028     C*----------------------------------------------------*
027900141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028000141028     C*----------------------------------------------------*
028100141028     C     INZVAR        BEGSR
028200141028     C*
028300141028     C* Inizializzo variabili di wrk
028400141028     C                   Z-ADD     *zeros        Num5_0            5 0
028500141028     C                   MOVEL     '0'           wFlgCAS
028600141028     C                   MOVEL     *blanks       wVATNOT_A
028700141028     C                   MOVEL     *blanks       wVATNOT_B
028800141028     C                   MOVEL     *blanks       wVATNOT_E
028900141028     C                   MOVEL     *blanks       wVATNOT_I
029000141028     C                   MOVEL     *blanks       wVATNOT_J
029100141028     C                   MOVEL     *blanks       wVATNOT_S
029200141028     C                   MOVEL     *blanks       wVATNOT_P
029300141028     C                   CLEAR                   SkSplitCSV_5
029400150527     C                   MOVEL     *blanks       wNOTE
029500141028     C*
029600141028     C* Reimposto i valori di default
029700141028     C                   EXSR      DEFCAM
029800141028     C*
029900141028     C                   ENDSR
030000141028     C*----------------------------------------------------*
030100141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
030200141028     C*----------------------------------------------------*
030300141028     C     DEFCAM        BEGSR
030400141028     C*
030500141028     C                   CLEAR                   EDIVAB00
030600141028     C                   CLEAR                   EDIVAT00
030700141028     C*
030800141028     C* Imposto i valori bolla di default
030900141028     C                   EVAL      VABCTR = 000
031000141028     C                   EVAL      VABCBO = '1'
031100141104     C                   EVAL      VABTSP = 'C'
031200141028     C*
031300141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
031400141028     C* e delimitatore testo.
031500141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
031600141028     C*
031700141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031800141028     C                   EVAL      CharSOS = CharNUM
031900141028     C*
032000141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
032100141028     C                   SETOFF                                       53
032200141028     C                   SELECT
032300141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
032400141028     C                   SETON                                        53
032500141028     C                   ENDSL
032600141028     C*
032700141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032800141028     C                   EVAL      posDaDft = 1
032900141028     C                   EVAL      posADft  = 0
033000141028     C                   EVAL      wGiroDft = 0
033100141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
033200141028     C                             posDaDft > 0
033300141028     C*
033400141028     C* Gestisco il 1° giro
033500141028     C                   IF        wGiroDft = 0
033600141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
033700141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033800141028     C* Incremento il contatore dei "giri"
033900141028     C                   EVAL      wGiroDft = 1
034000141028     C                   ELSE
034100141028     C                   EVAL      posDaDft = posADft
034200141028     C                   ENDIF
034300141028     C* Eseguo lo scan x trovare la fine del campo corrente
034400141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
034500141028     C*
034600141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
034700141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
034800141028     C                   IF        posDaDft > 0 AND
034900141028     C                             posADft  > 0
035000141028     C* NCL
035100141028     C                   IF        %subst(
035200141028     C                             %subst(vlrppt:posDaDft+1:
035300141028     C                             posADft-posDaDft-1):1:3)
035400141028     C                             = 'NCL'
035500141028     C                   EVAL      PiStr=%trim(%subst(
035600141028     C                             %subst(vlrppt:posDaDft+1:
035700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035800141028     C                   EXSR      CHKNUM
035900141028     C                   IF        PiInt=*on
036000141028     C                   Z-ADD     PiVal         VABNCL
036100141028     C                   ENDIF
036200141028     C                   ENDIF
036300141028     C* CCM
036400141028     C                   IF        %subst(
036500141028     C                             %subst(vlrppt:posDaDft+1:
036600141028     C                             posADft-posDaDft-1):1:3)
036700141028     C                             = 'CCM'
036800141028     C                   EVAL      PiStr=%trim(%subst(
036900141028     C                             %subst(vlrppt:posDaDft+1:
037000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037100141028     C                   EXSR      CHKNUM
037200141028     C                   IF        PiInt=*on
037300141028     C                   Z-ADD     PiVal         VABCCM
037400141028     C                   ENDIF
037500141028     C                   ENDIF
037600141028     C* LNP
037700141028     C                   IF        %subst(
037800141028     C                             %subst(vlrppt:posDaDft+1:
037900141028     C                             posADft-posDaDft-1):1:3)
038000141028     C                             = 'LNP'
038100141028     C                   EVAL      PiStr=%trim(%subst(
038200141028     C                             %subst(vlrppt:posDaDft+1:
038300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038400141028     C                   EXSR      CHKNUM
038500141028     C                   IF        PiInt=*on
038600141028     C                   Z-ADD     PiVal         VABLNP
038700141028     C                   ENDIF
038800141028     C                   ENDIF
038900141028     C* NRS
039000141028     C                   IF        %subst(
039100141028     C                             %subst(vlrppt:posDaDft+1:
039200141028     C                             posADft-posDaDft-1):1:3)
039300141028     C                             = 'NRS'
039400141028     C                   EVAL      PiStr=%trim(%subst(
039500141028     C                             %subst(vlrppt:posDaDft+1:
039600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039700141028     C                   EXSR      CHKNUM
039800141028     C                   IF        PiInt=*on
039900141028     C                   Z-ADD     PiVal         VABNRS
040000141028     C                   ENDIF
040100141028     C                   ENDIF
040200141028     C* CTR
040300141028     C                   IF        %subst(
040400141028     C                             %subst(vlrppt:posDaDft+1:
040500141028     C                             posADft-posDaDft-1):1:3)
040600141028     C                             = 'CTR'
040700141028     C                   EVAL      PiStr=%trim(%subst(
040800141028     C                             %subst(vlrppt:posDaDft+1:
040900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041000141028     C                   EXSR      CHKNUM
041100141028     C                   IF        PiInt=*on
041200141028     C                   Z-ADD     PiVal         VABCTR
041300141028     C                   ENDIF
041400141028     C                   ENDIF
041500170704     C* CTE
041600170704     C                   IF        %subst(
041700170704     C                             %subst(vlrppt:posDaDft+1:
041800170704     C                             posADft-posDaDft-1):1:3)
041900170704     C                             = 'CTE'
042000170704     C                   EVAL      PiStr=%trim(%subst(
042100170704     C                             %subst(vlrppt:posDaDft+1:
042200170704     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042300170704     C                   EXSR      CHKNUM
042400170704     C                   IF        PiInt=*on
042500170704     C                   Z-ADD     PiVal         wVABCTE
042600170704     C                   ENDIF
042700170704     C                   ENDIF
042800141028     C* PKB
042900141028     C                   IF        %subst(
043000141028     C                             %subst(vlrppt:posDaDft+1:
043100141028     C                             posADft-posDaDft-1):1:3)
043200141028     C                             = 'PKB'
043300141028     C                   EVAL      PiStr=%trim(%subst(
043400141028     C                             %subst(vlrppt:posDaDft+1:
043500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600141028     C                   EXSR      CHKNUM
043700141028     C                   IF        PiNum=*on
043800141028     C                   Z-ADD     PiVal         VABPKB
043900141028     C                   ENDIF
044000141028     C                   ENDIF
044100141028     C* VLB
044200141028     C                   IF        %subst(
044300141028     C                             %subst(vlrppt:posDaDft+1:
044400141028     C                             posADft-posDaDft-1):1:3)
044500141028     C                             = 'VLB'
044600141028     C                   EVAL      PiStr=%trim(%subst(
044700141028     C                             %subst(vlrppt:posDaDft+1:
044800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044900141028     C                   EXSR      CHKNUM
045000141028     C                   IF        PiNum=*on
045100141028     C                   Z-ADD     PiVal         VABVLB
045200141028     C                   ENDIF
045300141028     C                   ENDIF
045400141028     C* QFT
045500141028     C                   IF        %subst(
045600141028     C                             %subst(vlrppt:posDaDft+1:
045700141028     C                             posADft-posDaDft-1):1:3)
045800141028     C                             = 'QFT'
045900141028     C                   EVAL      PiStr=%trim(%subst(
046000141028     C                             %subst(vlrppt:posDaDft+1:
046100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046200141028     C                   EXSR      CHKNUM
046300141028     C                   IF        PiNum=*on
046400141028     C                   Z-ADD     PiVal         VABQFT
046500141028     C                   ENDIF
046600141028     C                   ENDIF
046700141028     C* CBO
046800141028     C                   IF        %subst(
046900141028     C                             %subst(vlrppt:posDaDft+1:
047000141028     C                             posADft-posDaDft-1):1:3)
047100141028     C                             = 'CBO'
047200141028     C                   EVAL      VABCBO=%trim(%subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047500141028     C                   ENDIF
047600141028     C* TSP
047700141028     C                   IF        %subst(
047800141028     C                             %subst(vlrppt:posDaDft+1:
047900141028     C                             posADft-posDaDft-1):1:3)
048000141028     C                             = 'TSP'
048100141028     C                   EVAL      VABTSP=%trim(%subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400141028     C                   ENDIF
048500141028     C* VAS
048600141028     C                   IF        %subst(
048700141028     C                             %subst(vlrppt:posDaDft+1:
048800141028     C                             posADft-posDaDft-1):1:3)
048900141028     C                             = 'VAS'
049000141028     C                   EVAL      VABVAS=%trim(%subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300141028     C                   ENDIF
049400141028     C* VCA
049500141028     C                   IF        %subst(
049600141028     C                             %subst(vlrppt:posDaDft+1:
049700141028     C                             posADft-posDaDft-1):1:3)
049800141028     C                             = 'VCA'
049900141028     C                   EVAL      VABVCA=%trim(%subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200141028     C                   ENDIF
050300141028     C* TIC
050400141028     C                   IF        %subst(
050500141028     C                             %subst(vlrppt:posDaDft+1:
050600141028     C                             posADft-posDaDft-1):1:3)
050700141028     C                             = 'TIC'
050800141028     C                   EVAL      VABTIC=%trim(%subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100141028     C                   ENDIF
051200141028     C* GCA
051300141028     C                   IF        %subst(
051400141028     C                             %subst(vlrppt:posDaDft+1:
051500141028     C                             posADft-posDaDft-1):1:3)
051600141028     C                             = 'GCA'
051700141028     C                   EVAL      VABGCA=%trim(%subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000141028     C                   ENDIF
052100141028     C* CTM
052200141028     C                   IF        %subst(
052300141028     C                             %subst(vlrppt:posDaDft+1:
052400141028     C                             posADft-posDaDft-1):1:3)
052500141028     C                             = 'CTM'
052600141028     C                   EVAL      VABCTM=%trim(%subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900141028     C                   ENDIF
053000141028     C* FFD
053100141028     C                   IF        %subst(
053200141028     C                             %subst(vlrppt:posDaDft+1:
053300141028     C                             posADft-posDaDft-1):1:3)
053400141028     C                             = 'FFD'
053500141028     C                   EVAL      VABFFD=%trim(%subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800141028     C                   ENDIF
053900141028     C* VAD
054000141028     C                   IF        %subst(
054100141028     C                             %subst(vlrppt:posDaDft+1:
054200141028     C                             posADft-posDaDft-1):1:3)
054300141028     C                             = 'VAD'
054400141028     C                   EVAL      VABVAD=%trim(%subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700141028     C                   ENDIF
054800141028     C* GMA
054900141028     C                   IF        %subst(
055000141028     C                             %subst(vlrppt:posDaDft+1:
055100141028     C                             posADft-posDaDft-1):1:3)
055200141028     C                             = 'GMA'
055300141028     C                   EVAL      VABGMA=%trim(%subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600141028     C                   ENDIF
055700141028     C* GGA
055800141028     C                   IF        %subst(
055900141028     C                             %subst(vlrppt:posDaDft+1:
056000141028     C                             posADft-posDaDft-1):1:3)
056100141028     C                             = 'GGA'
056200141028     C                   EVAL      VABGGA=%trim(%subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500141028     C                   ENDIF
056600141028     C* GVA
056700141028     C                   IF        %subst(
056800141028     C                             %subst(vlrppt:posDaDft+1:
056900141028     C                             posADft-posDaDft-1):1:3)
057000141028     C                             = 'GVA'
057100141028     C                   EVAL      VABGVA=%trim(%subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400141028     C                   ENDIF
057500141028     C* TC1
057600141028     C                   IF        %subst(
057700141028     C                             %subst(vlrppt:posDaDft+1:
057800141028     C                             posADft-posDaDft-1):1:3)
057900141028     C                             = 'TC1'
058000141028     C                   EVAL      VABTC1=%trim(%subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300141028     C                   ENDIF
058400141028     C* TC2
058500141028     C                   IF        %subst(
058600141028     C                             %subst(vlrppt:posDaDft+1:
058700141028     C                             posADft-posDaDft-1):1:3)
058800141028     C                             = 'TC2'
058900141028     C                   EVAL      VABTC2=%trim(%subst(
059000141028     C                             %subst(vlrppt:posDaDft+1:
059100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200141028     C                   ENDIF
059300141028     C* VATTRC
059400141028     C                   IF        %subst(
059500141028     C                             %subst(vlrppt:posDaDft+1:
059600141028     C                             posADft-posDaDft-1):1:3)
059700141028     C                             = 'TRC'
059800141028     C                   EVAL      VATTRC=%trim(%subst(
059900141028     C                             %subst(vlrppt:posDaDft+1:
060000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100141028     C                   ENDIF
060200141028     C* ...
060300141028     C                   ENDIF
060400141028     C                   ENDDO
060500141028     C*
060600141028     C                   ENDSR
060700000801     C*----------------------------------------------------*
060800140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
060900000801     C*----------------------------------------------------*
061000040910     C     IMPVABVAT     BEGSR
061100040910     C*
061200040910     C* Traduzione relativa ai tipi record del file del cliente
061300040910     C*
061400071210     C*
061500071210     C***
061600141028     C* ...tipo record 'UNB' (info CMR)
061700141028     C                   EVAL      wTag = 'UNB'
061800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061900141028     C                   MOVEL     *blanks       savCMR           35
062000141028     C                   EVAL      InStringa =
062100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
062200141028     C                   EVAL      InSepara = '+'
062300141028     C                   EXSR      SR_SPLIT
062400141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
062500140922     C                                      %char(datcor) + ' ' + %char(oracor)
062600130620     C                   ENDIF
062700141028     C*
062800141028     C***
062900150526     C* ...tipo record 'CNI' (rottuta di codice spedizione)
063000150526     C                   EVAL      wTag = 'CNI'
063100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
063200150526     C*
063300150526     C* Scarico buffer spedizione elaborata
063400150526     C                   IF        *IN90
063500150526     C  N31              ADD       1             §CTROKVB
063600150526     C  N31              exsr      CHKWRI
063700150526     C  N31              WRITE     EDIVAB00
063800150526     C                   ENDIF
063900150526     C                   SETON                                        90
064000150526     C*
064100150526     C* Inizializzazioni per nuova spedizione
064200141028     C                   SETOFF                                       31
064300141028     C                   eval      vinflg = '1'
064400141028     C* ......inizializzazioni iniziali e formati record file Bartolini
064500141028     C                   EXSR      INZVAR
064600141028     C* ......valorizzazione campi fissi
064700141028     C                   MOVEL     datcor        VABAAS
064800141028     C                   MOVE      datcor        VABMGS
064900141028     C                   MOVE(P)   vlrpoi        VABFGS
065000141028     C                   EVAL      VABCMR = savCMR
065100141028     C                   EVAL      VABDCM = datcor
065200141028     C                   EVAL      VABDTS = datcor
065300141028     C                   EVAL      VABCNT = 1
065400141028     C* ......NSP => Stacco un numeratore da AZNUM
065500141028     C                   clear                   TRUL33DS
065600141028     C                   eval      I33OPE = *zeros
065700141028     C                   eval      I33CNU = 302
065800141028     C                   eval      I33NUM = 1
065900141028     C                   movel     TRUL33DS      KPJBU
066000141028     C                   call      'TRUL33R'
066100141028     C                   parm                    KPJBA
066200141028     C                   movel     KPJBU         TRUL33DS
066300141028     C                   if        O33ERR = *zeros
066400141028     C                   z-add     O33NRF        VABNSP
066500141028     C                   else
066600141028     C                   Z-ADD     1             wRecErr
066700141028     C                   SETON                                        31
066800141028     C                   EVAL      vinmsg = %trimr(vinmsg)
066900141028     C                             + ' ' + 'VABNSP VATNSP'
067000141028     C                   endif
067100141028     C                   ENDIF
067200141028     C*
067300141028     C***
067400150629     C* ...tipo record 'RFF+AGE' (riferimento spedizione)
067500150629     C                   EVAL      wTag = 'RFF+AGE'
067600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067700150526     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
067800150526     C                   EXSR      CHKNUM
067900150526     C                   IF        PiInt=*on
068000150526     C                   Z-ADD     PiVal         VABRMN
068100150526     C                   ELSE
068200150526     C                   EVAL      VABRMN = 1
068300150526     C                   Z-ADD     1             wRecErr
068400150526     C                   ENDIF
068500141028     C                   ENDIF
068600150526     C*
068700150526     C***
068800150629     C* ...tipo record 'RFF+CU' (riferimento spedizione)
068900150629     C                   EVAL      wTag = 'RFF+CU'
069000150526     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069100150526     C                   EVAL      VABRMA =
069200150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
069300150526     C                   ENDIF
069400150527     C*
069500150527     C***
069600150527     C* ...tipo record 'DTM+2' (data consegna richiesta)
069700150527     C                   EVAL      wTag = 'DTM+2'
069800150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069900150527     C                   EVAL      PiStr=%subst(
070000150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
070100150527     C                   EXSR      CHKNUM
070200150527     C                   IF        PiInt=*on
070300150527     C                   Z-ADD     PiVal         VABDCR
070400150527     C                   ELSE
070500150527     C                   Z-ADD     1             wRecErr
070600150527     C                   ENDIF
070700150527     C                   ENDIF
070800141028     C*
070900141028     C***
071000141028     C* ...tipo record 'NAD+CN' (destinatario)
071100141028     C                   EVAL      wTag = 'NAD+CN'
071200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071300141028     C                   EVAL      InStringa =
071400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
071500141028     C                   EVAL      InSepara = '+'
071600141028     C                   EXSR      SR_SPLIT
071700141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
071800141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
071900141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
072000141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
072100141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
072200141028     C                   IF        VABNZD = 'I'   OR
072300141028     C                             VABNZD = 'IT'  OR
072400141028     C                             VABNZD = 'ITA'
072500141028     C                   EVAL      VABNZD = *blanks
072600141028     C                   ENDIF
072700141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
072800141028     C                   EVAL      InSepara = ':'
072900141028     C                   EXSR      SR_SPLIT
073000141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
073100141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
073200141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
073300141028     C                                      %trim(SkSplitCSV_5(4))+
073400141028     C                                      %trim(SkSplitCSV_5(5))
073500170704     C*
073600170704     C* Se spedizioe export imposto tariffa DPD
073700170704     C                   IF        VABNZD <> *blanks
073800170704     C                   EVAL      VABCTR = wVABCTE
073900170704     C                   ENDIF
074000170704     C*
074100141028     C                   ENDIF
074200150609     C*
074300150609     C***
074400150609     C* ...tipo record 'NAD+CZ' (mittente originale)
074500150609     C                   EVAL      wTag = 'NAD+CZ'
074600150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074700150609     C                   EVAL      InStringa =
074800150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
074900150609     C                   EVAL      InSepara = '+'
075000150609     C                   EXSR      SR_SPLIT
075100150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
075200150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
075300150609     C                   IF        VABNMO = 'I'   OR
075400150609     C                             VABNMO = 'IT'  OR
075500150609     C                             VABNMO = 'ITA'
075600150609     C                   EVAL      VABNMO = *blanks
075700150609     C                   ENDIF
075800150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
075900150609     C                   EVAL      InSepara = ':'
076000150609     C                   EXSR      SR_SPLIT
076100150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
076200150609     C                   ENDIF
076300141104     C*
076400141104     C***
076500141104     C* ...tipo record 'NAD+UC' (destinatario)
076600141104     C                   EVAL      wTag = 'NAD+UC'
076700141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076800141104     C                   EVAL      InStringa =
076900141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
077000141104     C                   EVAL      InSepara = '+'
077100141104     C                   EXSR      SR_SPLIT
077200141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
077300141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
077400141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
077500141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
077600141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
077700141104     C                   IF        VABNZD = 'I'   OR
077800141104     C                             VABNZD = 'IT'  OR
077900141104     C                             VABNZD = 'ITA'
078000141104     C                   EVAL      VABNZD = *blanks
078100141104     C                   ENDIF
078200141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
078300141104     C                   EVAL      InSepara = ':'
078400141104     C                   EXSR      SR_SPLIT
078500141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
078600141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
078700141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
078800141104     C                                      %trim(SkSplitCSV_5(4))+
078900141104     C                                      %trim(SkSplitCSV_5(5))
079000141104     C                   ENDIF
079100141028     C*
079200141028     C***
079300141028     C* ...tipo record 'GID' (colli)
079400150609     C                   EVAL      wTag = 'GID+'
079500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079600141028     C                   EVAL      InStringa =
079700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079800141028     C                   EVAL      InSepara = '+'
079900141028     C                   EXSR      SR_SPLIT
080000150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
080100150526     C                   EVAL      InSepara = ':'
080200150526     C                   EXSR      SR_SPLIT
080300150526     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
080400141028     C                   EXSR      CHKNUM
080500141028     C                   IF        PiInt=*on
080600150526     C                   ADD       PiVal         VABNCL
080700141028     C                   ELSE
080800141028     C                   Z-ADD     1             wRecErr
080900141028     C                   ENDIF
081000141028     C                   ENDIF
081100150527     C*
081200150527     C***
081300170704     C* ...tipo record 'MOA+AAA' (contrassegno / divisa)
081400150527     C                   EVAL      wTag = 'MOA+AAA'
081500150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081600150527     C                   EVAL      InStringa =
081700150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
081800150527     C                   EVAL      InSepara = ':'
081900150527     C                   EXSR      SR_SPLIT
082000170918     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks AND
082100170918     C                             %trim(SkSplitCSV_5(1)) <> *zeros
082200170918     C                   EVAL      wFlgCAS = '1'
082300150527     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
082400150527     C                   EXSR      CHKNUM
082500150527     C                   IF        PiNum=*on
082600150527     C                   ADD       PiVal         VABCAS
082700150527     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
082800150527     C                   ELSE
082900150527     C                   Z-ADD     1             wRecErr
083000150527     C                   ENDIF
083100170918     C                   ENDIF
083200150527     C                   ENDIF
083300170704     C*
083400170704     C***
083500170704     C* ...tipo record 'MOA+22' (contrassegno / divisa)
083600170704     C                   EVAL      wTag = 'MOA+22'
083700170704     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083800170704     C                   EVAL      InStringa =
083900170704     C                                %subst(wvindta:%len(%trim(wTag))+2)
084000170704     C                   EVAL      InSepara = ':'
084100170704     C                   EXSR      SR_SPLIT
084200170918     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks AND
084300170918     C                             %trim(SkSplitCSV_5(1)) <> *zeros
084400170918     C                   EVAL      wFlgCAS = '1'
084500170704     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
084600170704     C                   EXSR      CHKNUM
084700170704     C                   IF        PiNum=*on
084800170704     C                   ADD       PiVal         VABCAS
084900170704     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
085000170704     C                   ELSE
085100170704     C                   Z-ADD     1             wRecErr
085200170704     C                   ENDIF
085300170918     C                   ENDIF
085400170704     C                   ENDIF
085500141028     C*
085600141028     C***
085700141028     C* ...tipo record 'FTX+AAA' (natura merce)
085800141028     C                   EVAL      wTag = 'FTX+AAA'
085900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086000141028     C                   EVAL      InStringa =
086100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
086200141028     C                   EVAL      InSepara = '+'
086300141028     C                   EXSR      SR_SPLIT
086400141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
086500141028     C                   ENDIF
086600150622     C*
086700150622     C***
086800150622     C* ...tipo record 'TSR+' (tipo servizio)
086900150622     C                   EVAL      wTag = 'TSR+'
087000150622     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087100150622     C                   EVAL      InStringa =
087200150622     C                                %subst(wvindta:%len(%trim(wTag))+2)
087300150622     C                   EVAL      InSepara = '+'
087400150622     C                   EXSR      SR_SPLIT
087500150622     C                   SELECT
087600150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = '3'
087700150622     C                   EVAL      VABTSP = 'C'
087800150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'E'
087900150622     C                   EVAL      VABTSP = 'E'
088000150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'H'
088100150622     C                   EVAL      VABTSP = 'H'
088200150622     C                   ENDSL
088300150622     C                   ENDIF
088400150527     C*
088500150527     C***
088600150527     C* ...tipo record 'FTX+DIN' (note di consegna)
088700150527     C                   EVAL      wTag = 'FTX+DIN'
088800150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088900150527     C                   EVAL      InStringa =
089000150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
089100150527     C                   EVAL      InSepara = '+'
089200150527     C                   EXSR      SR_SPLIT
089300150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
089400150527     C                                     %trim(SkSplitCSV_5(3))
089500150527     C                   ENDIF
089600150527     C*
089700150527     C***
089800150527     C* ...tipo record 'FTX+ACB' (note aggiuntive)
089900150527     C                   EVAL      wTag = 'FTX+ACB'
090000150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090100150527     C                   EVAL      InStringa =
090200150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
090300150527     C                   EVAL      InSepara = '+'
090400150527     C                   EXSR      SR_SPLIT
090500150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
090600150527     C                                     %trim(SkSplitCSV_5(3))
090700150527     C                   ENDIF
090800150609     C*
090900150609     C***
091000150609     C* ...tipo record 'FTX+AAI' (telefono destinatario / note)
091100150609     C                   EVAL      wTag = 'FTX+AAI'
091200150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
091300150609     C                   EVAL      InStringa =
091400150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
091500150609     C                   EVAL      InSepara = '+'
091600150609     C                   EXSR      SR_SPLIT
091700150609     C                   MOVEL     *blanks       wSplitCSV_5     256
091800150609     C                   EVAL      wSplitCSV_5 = SkSplitCSV_5(3)
091900150609     C     minu:maiu     XLATE     wSplitCSV_5   wSplitCSV_5                    *Minus -> Maiuscolo
092000150609     C                   IF        %scan('PHONE':wSplitCSV_5) > *zeros
092100150609     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
092200150609     C                                            %scan('PHONE':wSplitCSV_5)+
092300150609     C                                            %len('PHONE')+1))
092400150609     C                   EVAL      VATTRC = 'B'
092500150609     C                   EXSR      WRIVAT
092600150609     C                   ELSE
092700150609     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
092800150609     C                                     %trim(SkSplitCSV_5(3))
092900150609     C                   ENDIF
093000150609     C                   ENDIF
093100150629     C*
093200150629     C***
093300150629     C* ...tipo record 'MEA+WT+AAG' (peso spedizione)
093400150629     C                   EVAL      wTag = 'MEA+WT+AAG'
093500150629     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093600150629     C                   EVAL      InStringa =
093700150629     C                                %subst(wvindta:%len(%trim(wTag))+2)
093800150629     C                   EVAL      InSepara = ':'
093900150629     C                   EXSR      SR_SPLIT
094000150629     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
094100150629     C                   EXSR      CHKNUM
094200150629     C                   IF        PiNum=*on
094300150629     C                   ADD       PiVal         VABPKB
094400150629     C                   ELSE
094500150629     C                   Z-ADD     1             wRecErr
094600150629     C                   ENDIF
094700150629     C                   ENDIF
094800150629     C*
094900150629     C***
095000150629     C* ...tipo record 'MEA+WT+AAE' (peso spedizione)
095100150629     C                   EVAL      wTag = 'MEA+WT+AAE'
095200150629     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095300150629     C                   EVAL      InStringa =
095400150629     C                                %subst(wvindta:%len(%trim(wTag))+2)
095500150629     C                   EVAL      InSepara = ':'
095600150629     C                   EXSR      SR_SPLIT
095700150629     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
095800150629     C                   EXSR      CHKNUM
095900150629     C                   IF        PiNum=*on
096000150629     C                   ADD       PiVal         VABPKB
096100150629     C                   ELSE
096200150629     C                   Z-ADD     1             wRecErr
096300150629     C                   ENDIF
096400150629     C                   ENDIF
096500141028     C*
096600141028     C***
096700141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
096800141028     C                   EVAL      wTag = 'MEA+VOL'
096900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097000141028     C                   EVAL      InStringa =
097100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
097200141028     C                   EVAL      InSepara = ':'
097300141028     C                   EXSR      SR_SPLIT
097400141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
097500141028     C                   EXSR      CHKNUM
097600141028     C                   IF        PiNum=*on
097700150924     C                   ADD       PiVal         VABVLB
097800141028     C                   ELSE
097900141028     C                   Z-ADD     1             wRecErr
098000141028     C                   ENDIF
098100141028     C                   ENDIF
098200170704     C*
098300170704     C***
098400170704     C* ...tipo record 'CTA+IC (referente consegna)
098500170704     C                   EVAL      wTag = 'CTA+IC'
098600170704     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
098700170704     C                   EVAL      InStringa =
098800170704     C                                %subst(wvindta:%len(%trim(wTag))+2)
098900170704     C                   EVAL      InStringa = %trim(SkSplitCSV_5(1))
099000170704     C                   EVAL      InSepara = ':'
099100170704     C                   EXSR      SR_SPLIT
099200170704     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
099300170704     C                   EVAL      VATTRC = 'B'
099400170704     C                   EXSR      WRIVAT
099500170704     C                   ENDIF
099600141028     C*
099700141028     C***
099800150709     C* ...tipo record 'COM:TE (numero telefono referente consegna)
099900150709     C                   EVAL      wTag = 'COM'
100000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100100150526     C                   EVAL      InStringa =
100200150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
100300150526     C                   EVAL      InSepara = '+'
100400150526     C                   EXSR      SR_SPLIT
100500150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
100600150707     C                   EVAL      InStringa = %trim(SkSplitCSV_5(1))
100700150707     C                   EVAL      InSepara = ':'
100800150707     C                   EXSR      SR_SPLIT
100900150707     C                   IF        %trim(SkSplitCSV_5(2)) = 'TE'
101000150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
101100150707     C                   EVAL      VATTRC = 'B'
101200141028     C                   EXSR      WRIVAT
101300150707     C                   ENDIF
101400150526     C                   ENDIF
101500141028     C                   ENDIF
101600150707     C*
101700150707     C***
101800150707     C* ...tipo record 'PCI+24' (dettagli segnacolli)
101900150707     C                   EVAL      wTag = 'PCI+24'
102000150707     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102100150707     C                   EVAL      InStringa =
102200150707     C                                %subst(wvindta:%len(%trim(wTag))+2)
102300150707     C                   EVAL      InSepara = '+'
102400150707     C                   EXSR      SR_SPLIT
102500150707     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
102600150707     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
102700150707     C                   EVAL      VATTRC = 'E'
102800150707     C                   EXSR      WRIVAT
102900150707     C                   ENDIF
103000150707     C                   ENDIF
103100141028     C*
103200141028     C***
103300141028     C* ...tipo record '???' (PDF per packing-list)
103400141028     C                   EVAL      wTag = '???PDF'
103500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
103600141028     C*
103700141028     C* ......VATNOT_P
103800141028     C                   if        *in53
103900141028     C*
104000141028     C* Solo se valorizzato RMN
104100141028     C                   if        VABRMA <> *blanks
104200141028     C                   eval      pIn_MaskPDF =
104300141028     C                                     %trim(%subst(wvindta:1292:100))
104400141028     C                   call      'TIS7P2SER'
104500141028     C                   parm      'W'           pIn_Opz           1
104600141028     C                   parm                    tivlrds
104700141028     C                   parm                    EDIVABDS
104800141028     C                   parm      '10'          pIn_CdIdAz        2
104900141028     C                   parm                    pIn_MaskPDF      50
105000141028     C                   parm      *blanks       pOut_Esito        1
105100141028     C                   endif
105200141028     C                   endif
105300141028     C*
105400141028     C                   ENDIF
105500141028     C*
105600141028     C***
105700141028     C* ...tipo record 'UNT' (fine bolla)
105800141028     C                   EVAL      wTag = 'UNT'
105900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
106000150526     C                   IF        *IN90
106100141028     C  N31              ADD       1             §CTROKVB
106200141028     C  N31              exsr      CHKWRI
106300140117     C  N31              WRITE     EDIVAB00
106400150526     C                   ENDIF
106500150526     C                   ENDIF
106600010202     C*
106700000801     C* Ebbene...
106800000801     C                   ADD       1             §CTRMO
106900141028     C                   IF        wRecErr <> *zeros
107000000801     C                   ADD       1             §CTRNO
107100000801     C                   EVAL      vinflg = '2'
107200000801     C                   ENDIF
107300000801     C*
107400000801     C                   ENDSR
107500010202     C*----------------------------------------------------*
107600140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
107700010202     C*----------------------------------------------------*
107800020305     C     PREVAT        BEGSR
107900010202     C*
108000140117     C* Compongo il nome del membro da dare al EDIVATWR
108100010202     C                   eval      parmbr = vlrhdl
108200010202     C                   movel     'M'           parmbr
108300060113     C                   eval      parccm = vlrksc
108400010202     C                   eval      paropz = '1'
108500010202     C* Effettuo la chiamata al CLLE preposto
108600140117     C                   call(e)   'TITVEVTC'
108700010202     C                   parm                    parccm
108800010202     C                   parm                    parmbr
108900010202     C                   parm                    paropz
109000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
109100010202     C                   if        %error
109200010202     C                   movel     '1'           chkcall
109300010202     C                   else
109400010202     C                   movel     '0'           chkcall
109500010202     C                   endif
109600010202     C*
109700010202     C                   ENDSR
109800141028     C*----------------------------------------------------*
109900141028     C*  CONTROLLO NUMERICITA' CAMPI
110000141028     C*----------------------------------------------------*
110100141028     C     CHKNUM        BEGSR
110200141028     C*
110300141028     C                   IF        PiDecChr = *blanks
110400141028     C                   EVAL      PiDecChr = CharNUM
110500141028     C                   ENDIF
110600141028     C*
110700141028     C                   callp     UBISNUM_Check(PiStr
110800141028     C                                          :PiDecChr
110900141028     C                                          :PiVal
111000141028     C                                          :PiNum
111100141028     C                                          :PiInt)
111200141028     C*
111300141028     C                   ENDSR
111400141028     C***
111500141028
111600141028
111700141028     C*----------------------------------------------------*
111800141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
111900141028     C*----------------------------------------------------*
112000141028     C     SR_SPLIT      BEGSR
112100141028     C*
112200141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
112300141028     C                   CALL      'XSPLIT2'
112400141028     C                   PARM                    InStringa
112500141028     C                   PARM                    InSepara
112600141028     C                   PARM      '5'           InTypeOut
112700141028     C                   PARM      ''            wSkParam
112800141028     C                   PARM                    OutErrore
112900141028     C                   PARM                    OutMsg
113000141028     C                   MOVEA     wSkParam      SkSplitCSV_5
113100141028     C*
113200141028     C                   ENDSR
113300141028
113400141028
113500141028     C*----------------------------------------------------*
113600141028     C*  SCRITTURA BUFFER EDIVAT
113700141028     C*----------------------------------------------------*
113800141028     C     WRIVAT        BEGSR
113900141028     C*
114000141028     C                   IF        VATNOT <> *blanks
114100141028     C                   EVAL      VATFGS = VABFGS
114200141028     C                   EVAL      VATCCM = VABCCM
114300141028     C                   EVAL      VATLNP = VABLNP
114400141028     C                   EVAL      VATAAS = VABAAS
114500141028     C                   EVAL      VATNRS = VABNRS
114600141028     C                   EVAL      VATNSP = VABNSP
114700141028     C                   EVAL      VATCMR = VABCMR
114800141028     C                   EVAL      VATCNT = VABCNT
114900141028     C                   ADD       1             §CTROKVT
115000141028     C                   WRITE     EDIVAT00
115100141028     C                   ENDIF
115200141028     C*
115300141028     C                   ENDSR
115400000801
115500011113
115600011113     C*----------------------------------------------------*
115700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
115800011113     C*----------------------------------------------------*
115900011113     C     CHKIMPDIV     BEGSR
116000011113     C*
116100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
116200011113     C                   Z-ADD     *zeros        wrkDec            9 9
116300011113     C*
116400011113     C* Come prima cosa effettuo considerazioni sulla divisa
116500011113     C                   IF        vabIAS > *zeros
116600011113     C                   IF        vabVAS <> 'EUR'
116700011113     C                   EVAL      vabVAS =  'ITL'
116800011113     C                   ENDIF
116900011113     C                   ENDIF
117000011113     C*
117100011113     C                   IF        vabCAS > *zeros
117200011113     C                   IF        vabVCA <> 'EUR'
117300011113     C                   EVAL      vabVCA =  'ITL'
117400011113     C                   ENDIF
117500011113     C                   ENDIF
117600011113     C*
117700011113     C                   IF        vabVMD > *zeros
117800020305     C                   IF        vabVAD <> 'EUR'
117900011113     C                   EVAL      vabVAD =  'ITL'
118000011113     C                   ENDIF
118100011113     C                   ENDIF
118200011113     C*
118300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
118400011113     C                   Z-ADD     vabIAS        wrkDec
118500011113     C                   IF        wrkDec > *zeros
118600011113     C                   IF        vabVAS = 'ITL'
118700011113     C                   EVAL      vabIAS = *zeros
118800011113     C                   ENDIF
118900011113     C                   ENDIF
119000011113     C*
119100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
119200011113     C                   Z-ADD     vabCAS        wrkDec
119300011113     C                   IF        wrkDec > *zeros
119400011113     C                   IF        vabVCA = 'ITL'
119500011113     C                   EVAL      vabCAS = *zeros
119600011113     C                   ENDIF
119700011113     C                   ENDIF
119800011113     C*
119900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
120000011113     C                   Z-ADD     vabVMD        wrkDec
120100011113     C                   IF        wrkDec > *zeros
120200011113     C                   IF        vabVAD = 'ITL'
120300011113     C                   EVAL      vabVMD = *zeros
120400011113     C                   ENDIF
120500011113     C                   ENDIF
120600011113     C*
120700011113     C                   ENDSR
120800011113     C***
120900011113
121000011113
121100000801
121200000801
121300990920      /TITLE Invio dei dati al punto operativo.
121400010202     C     invio         BEGSR
121500990920     C*
121600140117     C* 1° invio EDIVAT
121700010201     C                   reset                   dscmz
121800010201     C                   move      vlrpoi        cmzdst
121900140117     C                   eval      cmzfld = 'EDIVATWR'
122000010201     C                   eval      cmzmbd = vlrhdl
122100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
122200021009     C***                if        prmfir = *blanks
122300140117     C                   eval      cmzfla = 'EDIVAT0F'
122400140117     C                   eval      cmzmba = 'EDIVAT0F'
122500021009     C***                else
122600021009     C***                eval      cmzfla = prmfir
122700021009     C***                eval      cmzmba = prmfir
122800021009     C***                endif
122900010201     C                   eval      cmznrr = *zeros
123000020305     C                   move      §ctrokvt      cmznrr
123100021018     C                   eval      cmzlba = vlrfl1
123200010201     C                   call(e)   'TIS711C'
123300010201     C                   parm                    dscmz
123400010201     C                   parm      *blanks       esito
123500010205     C                   if        %error
123600010205     C                             or cmzerr = '1'
123700010205     C                             or esito  = '1'
123800010205     C                   eval      wrkesito = '3'
123900010205     C                   else
124000010201     C*
124100140117     C* 2° invio EDIVAB
124200010201     C                   reset                   dscmz
124300010201     C                   move      vlrpoi        cmzdst
124400010201     C                   eval      cmzfld = vlrfou
124500010201     C                   eval      cmzmbd = vlrhdl
124600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
124700021009     C***                if        prmfir = *blanks
124800140117     C                   eval      cmzfla = 'EDIVAB0F'
124900140117     C                   eval      cmzmba = 'EDIVAB0F'
125000021009     C***                else
125100021009     C***                eval      cmzfla = prmfir
125200021009     C***                eval      cmzmba = prmfir
125300021009     C***                endif
125400010201     C                   eval      cmznrr = *zeros
125500010201     C                   move      §ctrokvb      cmznrr
125600021018     C                   eval      cmzlba = vlrfl1
125700010201     C                   call(e)   'TIS711C'
125800010201     C                   parm                    dscmz
125900010201     C                   parm      *blanks       esito
126000010201     C                   if        %error
126100010201     C                             or cmzerr = '1'
126200010201     C                             or esito  = '1'
126300010201     C                   eval      wrkesito = '3'
126400010201     C                   endif
126500010205     C                   endif
126600990920     C*
126700000613     C                   ENDSR
126800000613     C***
126900070411
127000141028
127100070411     C     *pssr         BEGSR
127200070411     C*
127300070411     C                   if        %open(tivin00r)
127400070411     C                   close     tivin00r
127500070411     C                   endif
127600140117     C                   if        %open(edivabwr)
127700140117     C                   close     edivabwr
127800070411     C                   endif
127900140117     C                   if        %open(edivatwr)
128000140117     C                   close     edivatwr
128100070411     C                   endif
128200070411     C*
128300070411     C* Effettuo la chiamata al CLLE preposto
128400140117     C                   call(e)   'TITVEVTC'
128500070411     C                   parm                    parccm
128600070411     C                   parm                    parmbr
128700070411     C                   parm      '2'           paropz
128800070411     C*
128900070411     C                   eval      wrkesito = '2'
129000070411     C*
129100070411     C                   seton                                        LR
129200070411     C*
129300070411     C                   ENDSR     '*CANCL'
129400070411     C***
129500070411
129600990910
129700000613     C     *inzsr        BEGSR
129800990910     C*
129900990910     C     *entry        plist
130000990920     C                   parm                    tivlrds
130100990921     C                   parm      wrkesito      esito
130200000724     C                   parm                    prmlit
130300000710     C                   parm                    prmfir
130400000613     C*
130500000830     C* CALCOLA LA DATA CORRENTE
130600140117     C                   time                    wn14             14 0
130700140117     C                   movel     wn14          oracor            6 0          *ORA
130800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
130900100722     C                   eval      datcor = %dec(%date() : *ISO)
131000000830     C*
131100000613     C                   ENDSR
131200000613     C***
