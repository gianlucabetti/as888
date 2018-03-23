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
001700140117     D edivabds      e ds                  extname(edivab0f)
001800161018     D ds15          e ds
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
003300161013     D wEMAIL          s             70    INZ(*blanks)
003400141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003900141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
004000141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004100161018
004200161018     D*------------
004300161018     D jNAZ            s              5  0 INZ(*zeros)
004400161018     D skNAZISO        S              3    DIM(1000)
004500161018     D skNAZBAR        S              3    DIM(1000)
004600141028
004700141028     D*------------------
004800141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900141028     D*------------------
005000141028     D CharNUM         S              1
005100141028     D CharSOS         S              1
005200141028     D posDaDft        S              3  0 INZ(*zeros)
005300141028     D posADft         S              3  0 INZ(*zeros)
005400141028     D wGiroDft        s              1  0 INZ(*zeros)
005500141028
005600141028
005700141028     D*------------------
005800141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005900141028     D*------------------
006000141028     D InStringa       S          65535A   VARYING                              (stringa input)
006100141028     D InSepara        S             10A                                        (separatore)
006200141028     D InTypeOut       S              1A                                        (tipo output)
006300141028     D wSkParam        S          65535A                                        (output)
006400141028     D OutErrore       S              1A                                        (flag errore)
006500141028     D OutMsg          S             80A                                        (messaggio errore)
006600141028     D SkSplitCSV_5    S            256    DIM(256)
006700141028
006800000830
006900041025     D*------------------
007000041025     D* DS REPERIMENTO NUMERATORE
007100041025     D*------------------
007200041025     D trul33ds      e ds                  inz
007300041025     D*------------------
007400041025     D* DS ARCHITETTURA
007500041025     D*------------------
007600041025     D kpjba         e ds                  inz
007700041025     D*------------------
007800990908
007900141028
008000141028     D*------------------
008100141028     D* LINKING A DEFINIZIONI ESTERNE
008200141028     D*------------------
008300141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008400141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008500150609
008600141028
008700150609     D*-------------------
008800150609     D* COSTANTI
008900150609     D*-------------------
009000150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200141028
009300010201
009400010201
009500000913     C                   reset                   rrnum
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800150526     C                   setoff                                       90
009900000613     C*
010000161018     C                   EXSR      CARTAB                                       CARICA TABELLE
010100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010200070719     C*
010300070719     C* Esegue lancio TISI95R solo x chiusura
010400070719     C                   CLEAR                   TISI95DS
010500070719     C                   EVAL      I95TLA = 'C'
010600070719     C                   CALL      'TISI95R'
010700070719     C                   PARM                    TISI95DS
010800000613     C*
010900010202     C* Effettuo la chiamata al CLLE preposto
011000140922     C                   call(e)   'TITVEVTC'
011100140922     C                   parm                    parccm
011200140922     C                   parm                    parmbr
011300140922     C                   parm      '2'           paropz
011400000616     C*
011500010201     C                   seton                                        LR
011600990908
011700000801
011800910830     C*--------------------------------------------------------
011900140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012000910830     C*--------------------------------------------------------
012100040526     C     RWFILE        BEGSR
012200990910     C*
012300990914     C                   if        not %open(tivin00r)
012400990908     C                   open      tivin00r
012500990914     C                   endif
012600140117     C                   if        not %open(edivabwr)
012700140117     C                   open      edivabwr
012800990914     C                   endif
012900141028     C*
013000140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
013100020305     C                   exsr      prevat
013200010201     C*
013300010202     C                   if        chkcall = '0'
013400010202     C*
013500140117     C                   if        not %open(edivatwr)
013600140117     C                   open      edivatwr
013700010201     C                   endif
013800990910     C*
013900010201     C                   clear                   §CTROKVB          5 0
014000020305     C                   clear                   §CTROKVT          5 0
014100000801     C                   clear                   §CTRMO            5 0
014200000801     C                   clear                   §CTRNO            5 0
014300141028     C*
014400141028     C* Inizializzazioni fuori ciclo
014500141028     C                   EXSR      INZVAR
014600140117     C*
014700140902     C                   if        *in53
014800140117     C                   call      'TIS7P2SER'
014900140117     C                   parm      'O'           pIn_Opz           1
015000140117     C                   parm                    tivlrds
015100140117     C                   parm                    EDIVABDS
015200140117     C                   parm      *blanks       pIn_CdIdAz        2
015300140117     C                   parm      *blanks       pIn_MaskPDF      50
015400140117     C                   parm      *blanks       pOut_Esito        1
015500140902     C                   endif
015600130708     C*
015700921023     C                   DO        *HIVAL
015800990913     C*
015900990915     C                   READ      tivin00r                               70
016000040910     C                   if        vindta > *blanks
016100000613     C                   add       1             rrnum
016200000801     C*
016300000801     C                   if        *in70 = *off
016400000801     C                             and
016500000801     C                             (vinflg = *blanks
016600000801     C                              or vinflg = '0'
016700000801     C                              or vinflg = '2')
016800000801     C*
016900000801     C                   clear                   vinmsg
017000141028     C*
017100141028     C* "normalizzo" il dato di input
017200141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
017300141028     C                                             %len(%trim(vindta))-1))
017400160209     C*
017500160209     C* Elimino le "duple di controllo EDIFACT"
017600160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017700160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
017800160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
017900160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018000160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
018100040910     C*
018200040910     C* Eseguo routine d traduzione
018300040910     C                   exsr      impvabvat
018400040802     C*
018500010305     C                   endif
018600000905     C*
018700000905     C                   else
018800000905     C                   eval      vinflg = '1'
018900000905     C                   endif
019000000905     C*
019100000905     C  N70              update    tivin000
019200000905     C*
019300991022     C  N70              ENDdo
019400100722     C*
019500100722     C* Scarico i buffer testata ancora "in canna"
019600140922     C*
019700141028     C***  N31              ADD       1             §CTROKVB
019800141028     C***  N31              exsr      CHKWRI
019900141028     C***  N31              WRITE     EDIVAB00
020000140117     C*
020100140902     C                   if        *in53
020200140117     C                   call      'TIS7P2SER'
020300140117     C                   parm      'C'           pIn_Opz           1
020400140117     C                   parm                    tivlrds
020500140117     C                   parm                    EDIVABDS
020600140117     C                   parm      *blanks       pIn_CdIdAz        2
020700140117     C                   parm      *blanks       pIn_MaskPDF      50
020800140117     C                   parm      *blanks       pOut_Esito        1
020900140902     C                   endif
021000010202     C*
021100010202     C                   endif
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800010201     C                   if        §ctrokvb > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800140117     C                   if        %open(edivabwr)
022900140117     C                   close     edivabwr
023000990914     C                   endif
023100140117     C                   if        %open(edivatwr)
023200140117     C                   close     edivatwr
023300010201     C                   endif
023400990910     C*
023500010201     C                   if        §ctrokvb > 0
023600000724     C                             and vlrpoi <> *zeros
023700010202     C                   exsr      invio
023800990920     C                   endif
023900990920     C*
024000910830     C                   ENDSR
024100000613     C***
024200140922
024300140922     C*----------------------------------------------------*
024400140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
024500140922     C*----------------------------------------------------*
024600140922     C     CHKWRI        BEGSR
024700140922     C*
024800141028     C                   if        VABRMN = *zeros
024900141028     C                   eval      VABRMN = VABNSP
025000141028     C                   endif
025100141028     C*
025200141028     C                   if        VABPRD = *blanks
025300141028     C* ......VABPRD
025400141028     C* Reperisco la provincia dal CAP e dalla località
025500141028     C                   IF        VABLOD <> *blanks AND
025600141028     C                             VABCAD <> *blanks AND
025700141028     C                             VABNZD  = *blanks
025800141028     C                   CLEAR                   TISI95DS
025900141028     C                   EVAL      I95TCN = '3'
026000141028     C                   Z-ADD     datcor        I95DAT
026100141028     C                   EVAL      I95CAP = VABCAD
026200141028     C                   EVAL      I95LOC = VABLOD
026300141028     C                   CALL      'TISI95R'
026400141028     C                   PARM                    TISI95DS
026500141028     C                   EVAL      VABPRD = O95PRV
026600141028     C                   ENDIF
026700141028     C                   endif
026800140922     C*
026900140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
027000141028     C                   IF        wFlgCAS <> '0'
027100140922     C                   IF        VABCBO = '1'
027200140922     C                   EVAL      VABCBO = '4'
027300140922     C                   ELSE
027400140922     C                   EVAL      VABCBO = '6'
027500140922     C                   ENDIF
027600140922     C                   ENDIF
027700140922     C*
027800140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027900141028     C                   EXSR      CHKIMPDIV
028000150527     C*
028100150527     C* Scompongo le note
028200150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
028300150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
028400140922     C*
028500140922     C                   ENDSR
028600141028     C*----------------------------------------------------*
028700141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800141028     C*----------------------------------------------------*
028900141028     C     INZVAR        BEGSR
029000141028     C*
029100141028     C* Inizializzo variabili di wrk
029200141028     C                   Z-ADD     *zeros        Num5_0            5 0
029300141028     C                   MOVEL     '0'           wFlgCAS
029400141028     C                   MOVEL     *blanks       wVATNOT_A
029500141028     C                   MOVEL     *blanks       wVATNOT_B
029600141028     C                   MOVEL     *blanks       wVATNOT_E
029700141028     C                   MOVEL     *blanks       wVATNOT_I
029800141028     C                   MOVEL     *blanks       wVATNOT_J
029900141028     C                   MOVEL     *blanks       wVATNOT_S
030000141028     C                   MOVEL     *blanks       wVATNOT_P
030100141028     C                   CLEAR                   SkSplitCSV_5
030200150527     C                   MOVEL     *blanks       wNOTE
030300141028     C*
030400141028     C* Reimposto i valori di default
030500141028     C                   EXSR      DEFCAM
030600141028     C*
030700141028     C                   ENDSR
030800141028     C*----------------------------------------------------*
030900141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
031000141028     C*----------------------------------------------------*
031100141028     C     DEFCAM        BEGSR
031200141028     C*
031300141028     C                   CLEAR                   EDIVAB00
031400141028     C                   CLEAR                   EDIVAT00
031500141028     C*
031600141028     C* Imposto i valori bolla di default
031700141028     C                   EVAL      VABCTR = 000
031800141028     C                   EVAL      VABCBO = '1'
031900141104     C                   EVAL      VABTSP = 'C'
032000141028     C*
032100141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032200141028     C* e delimitatore testo.
032300141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032400141028     C*
032500141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
032600141028     C                   EVAL      CharSOS = CharNUM
032700141028     C*
032800141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
032900141028     C                   SETOFF                                       53
033000141028     C                   SELECT
033100141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
033200141028     C                   SETON                                        53
033300141028     C                   ENDSL
033400141028     C*
033500141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
033600141028     C                   EVAL      posDaDft = 1
033700141028     C                   EVAL      posADft  = 0
033800141028     C                   EVAL      wGiroDft = 0
033900141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034000141028     C                             posDaDft > 0
034100141028     C*
034200141028     C* Gestisco il 1° giro
034300141028     C                   IF        wGiroDft = 0
034400141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
034500141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
034600141028     C* Incremento il contatore dei "giri"
034700141028     C                   EVAL      wGiroDft = 1
034800141028     C                   ELSE
034900141028     C                   EVAL      posDaDft = posADft
035000141028     C                   ENDIF
035100141028     C* Eseguo lo scan x trovare la fine del campo corrente
035200141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035300141028     C*
035400141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
035500141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
035600141028     C                   IF        posDaDft > 0 AND
035700141028     C                             posADft  > 0
035800141028     C* NCL
035900141028     C                   IF        %subst(
036000141028     C                             %subst(vlrppt:posDaDft+1:
036100141028     C                             posADft-posDaDft-1):1:3)
036200141028     C                             = 'NCL'
036300141028     C                   EVAL      PiStr=%trim(%subst(
036400141028     C                             %subst(vlrppt:posDaDft+1:
036500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036600141028     C                   EXSR      CHKNUM
036700141028     C                   IF        PiInt=*on
036800141028     C                   Z-ADD     PiVal         VABNCL
036900141028     C                   ENDIF
037000141028     C                   ENDIF
037100141028     C* CCM
037200141028     C                   IF        %subst(
037300141028     C                             %subst(vlrppt:posDaDft+1:
037400141028     C                             posADft-posDaDft-1):1:3)
037500141028     C                             = 'CCM'
037600141028     C                   EVAL      PiStr=%trim(%subst(
037700141028     C                             %subst(vlrppt:posDaDft+1:
037800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037900141028     C                   EXSR      CHKNUM
038000141028     C                   IF        PiInt=*on
038100141028     C                   Z-ADD     PiVal         VABCCM
038200141028     C                   ENDIF
038300141028     C                   ENDIF
038400141028     C* LNP
038500141028     C                   IF        %subst(
038600141028     C                             %subst(vlrppt:posDaDft+1:
038700141028     C                             posADft-posDaDft-1):1:3)
038800141028     C                             = 'LNP'
038900141028     C                   EVAL      PiStr=%trim(%subst(
039000141028     C                             %subst(vlrppt:posDaDft+1:
039100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039200141028     C                   EXSR      CHKNUM
039300141028     C                   IF        PiInt=*on
039400141028     C                   Z-ADD     PiVal         VABLNP
039500141028     C                   ENDIF
039600141028     C                   ENDIF
039700141028     C* NRS
039800141028     C                   IF        %subst(
039900141028     C                             %subst(vlrppt:posDaDft+1:
040000141028     C                             posADft-posDaDft-1):1:3)
040100141028     C                             = 'NRS'
040200141028     C                   EVAL      PiStr=%trim(%subst(
040300141028     C                             %subst(vlrppt:posDaDft+1:
040400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040500141028     C                   EXSR      CHKNUM
040600141028     C                   IF        PiInt=*on
040700141028     C                   Z-ADD     PiVal         VABNRS
040800141028     C                   ENDIF
040900141028     C                   ENDIF
041000141028     C* CTR
041100141028     C                   IF        %subst(
041200141028     C                             %subst(vlrppt:posDaDft+1:
041300141028     C                             posADft-posDaDft-1):1:3)
041400141028     C                             = 'CTR'
041500141028     C                   EVAL      PiStr=%trim(%subst(
041600141028     C                             %subst(vlrppt:posDaDft+1:
041700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041800141028     C                   EXSR      CHKNUM
041900141028     C                   IF        PiInt=*on
042000141028     C                   Z-ADD     PiVal         VABCTR
042100141028     C                   ENDIF
042200141028     C                   ENDIF
042300141028     C* PKB
042400141028     C                   IF        %subst(
042500141028     C                             %subst(vlrppt:posDaDft+1:
042600141028     C                             posADft-posDaDft-1):1:3)
042700141028     C                             = 'PKB'
042800141028     C                   EVAL      PiStr=%trim(%subst(
042900141028     C                             %subst(vlrppt:posDaDft+1:
043000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043100141028     C                   EXSR      CHKNUM
043200141028     C                   IF        PiNum=*on
043300141028     C                   Z-ADD     PiVal         VABPKB
043400141028     C                   ENDIF
043500141028     C                   ENDIF
043600141028     C* VLB
043700141028     C                   IF        %subst(
043800141028     C                             %subst(vlrppt:posDaDft+1:
043900141028     C                             posADft-posDaDft-1):1:3)
044000141028     C                             = 'VLB'
044100141028     C                   EVAL      PiStr=%trim(%subst(
044200141028     C                             %subst(vlrppt:posDaDft+1:
044300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044400141028     C                   EXSR      CHKNUM
044500141028     C                   IF        PiNum=*on
044600141028     C                   Z-ADD     PiVal         VABVLB
044700141028     C                   ENDIF
044800141028     C                   ENDIF
044900141028     C* QFT
045000141028     C                   IF        %subst(
045100141028     C                             %subst(vlrppt:posDaDft+1:
045200141028     C                             posADft-posDaDft-1):1:3)
045300141028     C                             = 'QFT'
045400141028     C                   EVAL      PiStr=%trim(%subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045700141028     C                   EXSR      CHKNUM
045800141028     C                   IF        PiNum=*on
045900141028     C                   Z-ADD     PiVal         VABQFT
046000141028     C                   ENDIF
046100141028     C                   ENDIF
046200141028     C* CBO
046300141028     C                   IF        %subst(
046400141028     C                             %subst(vlrppt:posDaDft+1:
046500141028     C                             posADft-posDaDft-1):1:3)
046600141028     C                             = 'CBO'
046700141028     C                   EVAL      VABCBO=%trim(%subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000141028     C                   ENDIF
047100141028     C* TSP
047200141028     C                   IF        %subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):1:3)
047500141028     C                             = 'TSP'
047600141028     C                   EVAL      VABTSP=%trim(%subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900141028     C                   ENDIF
048000141028     C* VAS
048100141028     C                   IF        %subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):1:3)
048400141028     C                             = 'VAS'
048500141028     C                   EVAL      VABVAS=%trim(%subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800141028     C                   ENDIF
048900141028     C* VCA
049000141028     C                   IF        %subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):1:3)
049300141028     C                             = 'VCA'
049400141028     C                   EVAL      VABVCA=%trim(%subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700141028     C                   ENDIF
049800141028     C* TIC
049900141028     C                   IF        %subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):1:3)
050200141028     C                             = 'TIC'
050300141028     C                   EVAL      VABTIC=%trim(%subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600141028     C                   ENDIF
050700141028     C* GCA
050800141028     C                   IF        %subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):1:3)
051100141028     C                             = 'GCA'
051200141028     C                   EVAL      VABGCA=%trim(%subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500141028     C                   ENDIF
051600141028     C* CTM
051700141028     C                   IF        %subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):1:3)
052000141028     C                             = 'CTM'
052100141028     C                   EVAL      VABCTM=%trim(%subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400141028     C                   ENDIF
052500141028     C* FFD
052600141028     C                   IF        %subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):1:3)
052900141028     C                             = 'FFD'
053000141028     C                   EVAL      VABFFD=%trim(%subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300141028     C                   ENDIF
053400141028     C* VAD
053500141028     C                   IF        %subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):1:3)
053800141028     C                             = 'VAD'
053900141028     C                   EVAL      VABVAD=%trim(%subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200141028     C                   ENDIF
054300141028     C* GMA
054400141028     C                   IF        %subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):1:3)
054700141028     C                             = 'GMA'
054800141028     C                   EVAL      VABGMA=%trim(%subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100141028     C                   ENDIF
055200141028     C* GGA
055300141028     C                   IF        %subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):1:3)
055600141028     C                             = 'GGA'
055700141028     C                   EVAL      VABGGA=%trim(%subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000141028     C                   ENDIF
056100141028     C* GVA
056200141028     C                   IF        %subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):1:3)
056500141028     C                             = 'GVA'
056600141028     C                   EVAL      VABGVA=%trim(%subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900141028     C                   ENDIF
057000141028     C* TC1
057100141028     C                   IF        %subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):1:3)
057400141028     C                             = 'TC1'
057500141028     C                   EVAL      VABTC1=%trim(%subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800141028     C                   ENDIF
057900141028     C* TC2
058000141028     C                   IF        %subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):1:3)
058300141028     C                             = 'TC2'
058400141028     C                   EVAL      VABTC2=%trim(%subst(
058500141028     C                             %subst(vlrppt:posDaDft+1:
058600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700141028     C                   ENDIF
058800141028     C* VATTRC
058900141028     C                   IF        %subst(
059000141028     C                             %subst(vlrppt:posDaDft+1:
059100141028     C                             posADft-posDaDft-1):1:3)
059200141028     C                             = 'TRC'
059300141028     C                   EVAL      VATTRC=%trim(%subst(
059400141028     C                             %subst(vlrppt:posDaDft+1:
059500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600141028     C                   ENDIF
059700141028     C* ...
059800141028     C                   ENDIF
059900141028     C                   ENDDO
060000141028     C*
060100141028     C                   ENDSR
060200000801     C*----------------------------------------------------*
060300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
060400000801     C*----------------------------------------------------*
060500040910     C     IMPVABVAT     BEGSR
060600040910     C*
060700040910     C* Traduzione relativa ai tipi record del file del cliente
060800040910     C*
060900071210     C*
061000071210     C***
061100141028     C* ...tipo record 'UNB' (info CMR)
061200141028     C                   EVAL      wTag = 'UNB'
061300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061400141028     C                   MOVEL     *blanks       savCMR           35
061500141028     C                   EVAL      InStringa =
061600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
061700141028     C                   EVAL      InSepara = '+'
061800141028     C                   EXSR      SR_SPLIT
061900141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
062000140922     C                                      %char(datcor) + ' ' + %char(oracor)
062100130620     C                   ENDIF
062200141028     C*
062300141028     C***
062400161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
062500161025     C                   EVAL      wTag = 'UNH'
062600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062700150526     C*
062800150526     C* Scarico buffer spedizione elaborata
062900150526     C                   IF        *IN90
063000150526     C  N31              ADD       1             §CTROKVB
063100150526     C  N31              exsr      CHKWRI
063200150526     C  N31              WRITE     EDIVAB00
063300150526     C                   ENDIF
063400150526     C                   SETON                                        90
063500150526     C*
063600150526     C* Inizializzazioni per nuova spedizione
063700141028     C                   SETOFF                                       31
063800141028     C                   eval      vinflg = '1'
063900141028     C* ......inizializzazioni iniziali e formati record file Bartolini
064000141028     C                   EXSR      INZVAR
064100141028     C* ......valorizzazione campi fissi
064200141028     C                   MOVEL     datcor        VABAAS
064300141028     C                   MOVE      datcor        VABMGS
064400141028     C                   MOVE(P)   vlrpoi        VABFGS
064500141028     C                   EVAL      VABCMR = savCMR
064600141028     C                   EVAL      VABDCM = datcor
064700141028     C                   EVAL      VABDTS = datcor
064800141028     C                   EVAL      VABCNT = 1
064900141028     C* ......NSP => Stacco un numeratore da AZNUM
065000141028     C                   clear                   TRUL33DS
065100141028     C                   eval      I33OPE = *zeros
065200141028     C                   eval      I33CNU = 302
065300141028     C                   eval      I33NUM = 1
065400141028     C                   movel     TRUL33DS      KPJBU
065500141028     C                   call      'TRUL33R'
065600141028     C                   parm                    KPJBA
065700141028     C                   movel     KPJBU         TRUL33DS
065800141028     C                   if        O33ERR = *zeros
065900141028     C                   z-add     O33NRF        VABNSP
066000141028     C                   else
066100141028     C                   Z-ADD     1             wRecErr
066200141028     C                   SETON                                        31
066300141028     C                   EVAL      vinmsg = %trimr(vinmsg)
066400141028     C                             + ' ' + 'VABNSP VATNSP'
066500141028     C                   endif
066600141028     C                   ENDIF
066700150527     C*
066800150527     C***
066900161103     C* ...tipo record 'DTM+2' (data consegna richiesta IL)
067000161103     C                   EVAL      wTag = 'DTM+2'
067100150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067200150527     C                   EVAL      PiStr=%subst(
067300150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
067400150527     C                   EXSR      CHKNUM
067500150527     C                   IF        PiInt=*on
067600150527     C                   Z-ADD     PiVal         VABDCR
067700161103     C***                EVAL      VABTCR = *blanks
067800150527     C                   ELSE
067900150527     C                   Z-ADD     1             wRecErr
068000150527     C                   ENDIF
068100150527     C                   ENDIF
068200161025     C*
068300161025     C***
068400161103     C* ...tipo record 'DTM+63' (data consegna richiesta ENTRO IL)
068500161103     C                   EVAL      wTag = 'DTM+63'
068600161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068700161025     C                   EVAL      PiStr=%subst(
068800161025     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
068900161025     C                   EXSR      CHKNUM
069000161025     C                   IF        PiInt=*on
069100161025     C                   Z-ADD     PiVal         VABDCR
069200161103     C***                EVAL      VABTCR = 'P'
069300161025     C                   ELSE
069400161025     C                   Z-ADD     1             wRecErr
069500161025     C                   ENDIF
069600161025     C                   ENDIF
069700161103     C*
069800161103     C***
069900161103     C* ...tipo record 'DTM+64' (data consegna richiesta DOPO IL)
070000161103     C                   EVAL      wTag = 'DTM+64'
070100161103     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070200161103     C                   EVAL      PiStr=%subst(
070300161103     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
070400161103     C                   EXSR      CHKNUM
070500161103     C                   IF        PiInt=*on
070600161103     C                   Z-ADD     PiVal         VABDCR
070700161103     C***                EVAL      VABTCR = 'D'
070800161103     C                   ELSE
070900161103     C                   Z-ADD     1             wRecErr
071000161103     C                   ENDIF
071100161103     C                   ENDIF
071200161025     C*
071300161025     C***
071400161025     C* ...tipo record 'MOA+22' (contrassegno / divisa)
071500161025     C                   EVAL      wTag = 'MOA+22'
071600161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071700161025     C                   EVAL      wFlgCAS = '1'
071800161025     C                   EVAL      InStringa =
071900161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
072000161025     C                   EVAL      InSepara = ':'
072100161025     C                   EXSR      SR_SPLIT
072200161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
072300161025     C                   EXSR      CHKNUM
072400161025     C                   IF        PiNum=*on
072500161025     C                   ADD       PiVal         VABCAS
072600161025     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
072700161025     C                   ELSE
072800161025     C                   Z-ADD     1             wRecErr
072900161025     C                   ENDIF
073000161025     C                   ENDIF
073100161103     C*
073200161103     C***
073300161103     C* ...tipo record 'FTX+AAA' (natura merce)
073400161103     C                   EVAL      wTag = 'FTX+AAA'
073500161103     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073600161103     C                   EVAL      InStringa =
073700161103     C                                %subst(wvindta:%len(%trim(wTag))+2)
073800161103     C                   EVAL      InSepara = '+'
073900161103     C                   EXSR      SR_SPLIT
074000161103     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
074100161103     C                   ENDIF
074200161025     C*
074300161025     C***
074400161103     C* ...tipo record 'FTX+DEL' (note di consegna)
074500161103     C                   EVAL      wTag = 'FTX+DEL'
074600161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074700161025     C                   EVAL      InStringa =
074800161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
074900161025     C                   EVAL      InSepara = '+'
075000161025     C                   EXSR      SR_SPLIT
075100161025     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
075200161025     C                                     %trim(SkSplitCSV_5(3))
075300161025     C                   ENDIF
075400161025     C*
075500161025     C***
075600161025     C* ...tipo record 'CNT+7' (peso spedizione)
075700161025     C                   EVAL      wTag = 'CNT+7'
075800161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075900161025     C                   EVAL      InStringa =
076000161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
076100161025     C                   EVAL      InSepara = ':'
076200161025     C                   EXSR      SR_SPLIT
076300161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
076400161025     C                   EXSR      CHKNUM
076500161025     C                   IF        PiNum=*on
076600161025     C                   Z-ADD     PiVal         VABPKB
076700161025     C                   ELSE
076800161025     C                   Z-ADD     1             wRecErr
076900161025     C                   ENDIF
077000161025     C                   ENDIF
077100161025     C*
077200161025     C***
077300161025     C* ...tipo record 'CNT+15' (volume spedizione)
077400161025     C                   EVAL      wTag = 'CNT+15'
077500161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077600161025     C                   EVAL      InStringa =
077700161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
077800161025     C                   EVAL      InSepara = ':'
077900161025     C                   EXSR      SR_SPLIT
078000161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
078100161025     C                   EXSR      CHKNUM
078200161025     C                   IF        PiNum=*on
078300161025     C                   Z-ADD     PiVal         VABVLB
078400161025     C                   ELSE
078500161025     C                   Z-ADD     1             wRecErr
078600161025     C                   ENDIF
078700161025     C                   ENDIF
078800161025     C*
078900161025     C***
079000161025     C* ...tipo record 'CNT+11' (numero colli)
079100161025     C                   EVAL      wTag = 'CNT+11'
079200161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079300161025     C                   EVAL      InStringa =
079400161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
079500161025     C                   EVAL      InSepara = ':'
079600161025     C                   EXSR      SR_SPLIT
079700161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
079800161025     C                   EXSR      CHKNUM
079900161025     C                   IF        PiInt=*on
080000161025     C                   Z-ADD     PiVal         VABNCL
080100161025     C                   ELSE
080200161025     C                   Z-ADD     1             wRecErr
080300161025     C                   ENDIF
080400161025     C                   ENDIF
080500161025     C*
080600161025     C***
080700161103     C* ...tipo record 'RFF+ACL' (riferimento spedizione)
080800161103     C                   EVAL      wTag = 'RFF+ACL'
080900161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081000161025     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
081100161103     C                   IF        PiStr<>*blanks
081200161025     C                   EXSR      CHKNUM
081300161025     C                   IF        PiInt=*on
081400161025     C                   Z-ADD     PiVal         VABRMN
081500161025     C                   ELSE
081600161025     C                   EVAL      VABRMN = 1
081700161025     C                   Z-ADD     1             wRecErr
081800161103     C                   ENDIF
081900161025     C                   ENDIF
082000161025     C                   ENDIF
082100161103     C*
082200161103     C***
082300161103     C* ...tipo record 'RFF+ACD' (riferimento spedizione)
082400161103     C                   EVAL      wTag = 'RFF+ACD'
082500161103     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082600161103     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
082700161103     C                   IF        PiStr<>*blanks
082800161103     C                   EXSR      CHKNUM
082900161103     C                   IF        PiInt=*on
083000161103     C                   Z-ADD     PiVal         VABRMN
083100161103     C                   ELSE
083200161103     C                   EVAL      VABRMN = 1
083300161103     C                   Z-ADD     1             wRecErr
083400161103     C                   ENDIF
083500161103     C                   ENDIF
083600161103     C                   ENDIF
083700161103     C*
083800161103     C***
083900161103     C* ...tipo record 'RFF+AAO' (riferimento spedizione)
084000161103     C                   EVAL      wTag = 'RFF+AAO'
084100161103     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
084200161103     C                   EVAL      InStringa =
084300161103     C                                %subst(wvindta:%len(%trim(wTag))+2)
084400161103     C                   EVAL      InSepara = ':'
084500161103     C                   EXSR      SR_SPLIT
084600161103     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
084700161103     C                   ENDIF
084800161103     C*
084900161103     C***
085000161103     C* ...tipo record 'CTA+IC' (referente consegna)
085100161103     C                   EVAL      wTag = 'CTA+IC'
085200161103     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085300161103     C                   EVAL      InStringa =
085400161103     C                                %subst(wvindta:%len(%trim(wTag))+2)
085500161103     C                   EVAL      InSepara = ':'
085600161103     C                   EXSR      SR_SPLIT
085700161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
085800161103     C                   EVAL      VATTRC = 'A'
085900161103     C                   EXSR      WRIVAT
086000161103     C                   ENDIF
086100141028     C*
086200141028     C***
086300141028     C* ...tipo record 'NAD+CN' (destinatario)
086400141028     C                   EVAL      wTag = 'NAD+CN'
086500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086600141028     C                   EVAL      InStringa =
086700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
086800141028     C                   EVAL      InSepara = '+'
086900141028     C                   EXSR      SR_SPLIT
087000141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
087100141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
087200141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
087300141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
087400141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
087500141028     C                   IF        VABNZD = 'I'   OR
087600141028     C                             VABNZD = 'IT'  OR
087700141028     C                             VABNZD = 'ITA'
087800141028     C                   EVAL      VABNZD = *blanks
087900141028     C                   ENDIF
088000141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
088100141028     C                   EVAL      InSepara = ':'
088200141028     C                   EXSR      SR_SPLIT
088300141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
088400141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
088500141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
088600141028     C                                      %trim(SkSplitCSV_5(4))+
088700141028     C                                      %trim(SkSplitCSV_5(5))
088800141028     C                   ENDIF
088900150609     C*
089000150609     C***
089100150609     C* ...tipo record 'NAD+CZ' (mittente originale)
089200150609     C                   EVAL      wTag = 'NAD+CZ'
089300150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089400150609     C                   EVAL      InStringa =
089500150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
089600150609     C                   EVAL      InSepara = '+'
089700150609     C                   EXSR      SR_SPLIT
089800150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
089900150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
090000150609     C                   IF        VABNMO = 'I'   OR
090100150609     C                             VABNMO = 'IT'  OR
090200150609     C                             VABNMO = 'ITA'
090300150609     C                   EVAL      VABNMO = *blanks
090400161018     C                   ELSE
090500161018     C                   Z-ADD     1             jNAZ
090600161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
090700161018     C                   IF        %found
090800161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
090900161018     C                   ENDIF
091000161018     C                   ENDIF
091100161018     C*
091200150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
091300150609     C                   EVAL      InSepara = ':'
091400150609     C                   EXSR      SR_SPLIT
091500150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
091600150609     C                   ENDIF
091700161013     C*
091800161025     C***
091900161013     C* ...tipo record 'COM' (email destinatario ai fini alert)
092000161013     C                   EVAL      wTag = 'COM'
092100161013     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092200161013     C                   EVAL      InStringa =
092300161013     C                                %subst(wvindta:%len(%trim(wTag))+2)
092400161013     C                   EVAL      InSepara = ':'
092500161013     C                   EXSR      SR_SPLIT
092600161013     C                   IF        SkSplitCSV_5(2) = 'EM'
092700161103     C                   IF        %len(%trim(SkSplitCSV_5(1))) > 4
092800161013     C                   EVAL      wEMAIL = SkSplitCSV_5(1)
092900161013     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
093000161013     C                   EVAL      VATTRC = 'I'
093100161013     C                   EXSR      WRIVAT
093200161013     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
093300161013     C                   EVAL      VATTRC = 'J'
093400161013     C                   EXSR      WRIVAT
093500161103     C                   ENDIF
093600161013     C                   ENDIF
093700161103     C                   IF        SkSplitCSV_5(2) = 'TE'
093800161103     C                   IF        %len(%trim(SkSplitCSV_5(1))) > 7
093900161103     C                   EVAL      VATNOT = SkSplitCSV_5(1)
094000161103     C                   EVAL      VATTRC = 'B'
094100161103     C                   EXSR      WRIVAT
094200161103     C                   ENDIF
094300161103     C                   ENDIF
094400161013     C                   ENDIF
094500141028     C*
094600141028     C***
094700161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
094800161013     C                   EVAL      wTag = 'GIN+BJ'
094900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095000150526     C                   EVAL      InStringa =
095100150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
095200161014     C     '+':':'       XLATE     InStringa     InStringa
095300161013     C                   EVAL      InSepara = ':'
095400150526     C                   EXSR      SR_SPLIT
095500150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
095600161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
095700141028     C                   EVAL      VATTRC = 'E'
095800141028     C                   EXSR      WRIVAT
095900150526     C                   ENDIF
096000150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
096100161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
096200150526     C                   EVAL      VATTRC = 'E'
096300150526     C                   EXSR      WRIVAT
096400150526     C                   ENDIF
096500150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
096600161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
096700150526     C                   EVAL      VATTRC = 'E'
096800150526     C                   EXSR      WRIVAT
096900150526     C                   ENDIF
097000150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
097100161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
097200150526     C                   EVAL      VATTRC = 'E'
097300150526     C                   EXSR      WRIVAT
097400150526     C                   ENDIF
097500150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
097600161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
097700150526     C                   EVAL      VATTRC = 'E'
097800150526     C                   EXSR      WRIVAT
097900150526     C                   ENDIF
098000161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
098100161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(6))
098200161017     C                   EVAL      VATTRC = 'E'
098300161017     C                   EXSR      WRIVAT
098400161017     C                   ENDIF
098500161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
098600161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(7))
098700161017     C                   EVAL      VATTRC = 'E'
098800161017     C                   EXSR      WRIVAT
098900161017     C                   ENDIF
099000161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
099100161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(8))
099200161017     C                   EVAL      VATTRC = 'E'
099300161017     C                   EXSR      WRIVAT
099400161017     C                   ENDIF
099500161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
099600161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(9))
099700161017     C                   EVAL      VATTRC = 'E'
099800161017     C                   EXSR      WRIVAT
099900161017     C                   ENDIF
100000161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
100100161103     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(10))
100200161017     C                   EVAL      VATTRC = 'E'
100300161017     C                   EXSR      WRIVAT
100400161017     C                   ENDIF
100500141028     C                   ENDIF
100600141028     C*
100700141028     C***
100800141028     C* ...tipo record '???' (PDF per packing-list)
100900141028     C                   EVAL      wTag = '???PDF'
101000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
101100141028     C*
101200141028     C* ......VATNOT_P
101300141028     C                   if        *in53
101400141028     C*
101500141028     C* Solo se valorizzato RMN
101600141028     C                   if        VABRMA <> *blanks
101700141028     C                   eval      pIn_MaskPDF =
101800141028     C                                     %trim(%subst(wvindta:1292:100))
101900141028     C                   call      'TIS7P2SER'
102000141028     C                   parm      'W'           pIn_Opz           1
102100141028     C                   parm                    tivlrds
102200141028     C                   parm                    EDIVABDS
102300141028     C                   parm      '10'          pIn_CdIdAz        2
102400141028     C                   parm                    pIn_MaskPDF      50
102500141028     C                   parm      *blanks       pOut_Esito        1
102600141028     C                   endif
102700141028     C                   endif
102800141028     C*
102900141028     C                   ENDIF
103000141028     C*
103100141028     C***
103200141028     C* ...tipo record 'UNT' (fine bolla)
103300141028     C                   EVAL      wTag = 'UNT'
103400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
103500150526     C                   IF        *IN90
103600141028     C  N31              ADD       1             §CTROKVB
103700141028     C  N31              exsr      CHKWRI
103800140117     C  N31              WRITE     EDIVAB00
103900150526     C                   ENDIF
104000150526     C                   ENDIF
104100010202     C*
104200000801     C* Ebbene...
104300000801     C                   ADD       1             §CTRMO
104400141028     C                   IF        wRecErr <> *zeros
104500000801     C                   ADD       1             §CTRNO
104600000801     C                   EVAL      vinflg = '2'
104700000801     C                   ENDIF
104800000801     C*
104900000801     C                   ENDSR
105000010202     C*----------------------------------------------------*
105100140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
105200010202     C*----------------------------------------------------*
105300020305     C     PREVAT        BEGSR
105400010202     C*
105500140117     C* Compongo il nome del membro da dare al EDIVATWR
105600010202     C                   eval      parmbr = vlrhdl
105700010202     C                   movel     'M'           parmbr
105800060113     C                   eval      parccm = vlrksc
105900010202     C                   eval      paropz = '1'
106000010202     C* Effettuo la chiamata al CLLE preposto
106100140117     C                   call(e)   'TITVEVTC'
106200010202     C                   parm                    parccm
106300010202     C                   parm                    parmbr
106400010202     C                   parm                    paropz
106500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
106600010202     C                   if        %error
106700010202     C                   movel     '1'           chkcall
106800010202     C                   else
106900010202     C                   movel     '0'           chkcall
107000010202     C                   endif
107100010202     C*
107200010202     C                   ENDSR
107300141028     C*----------------------------------------------------*
107400141028     C*  CONTROLLO NUMERICITA' CAMPI
107500141028     C*----------------------------------------------------*
107600141028     C     CHKNUM        BEGSR
107700141028     C*
107800141028     C                   IF        PiDecChr = *blanks
107900141028     C                   EVAL      PiDecChr = CharNUM
108000141028     C                   ENDIF
108100141028     C*
108200141028     C                   callp     UBISNUM_Check(PiStr
108300141028     C                                          :PiDecChr
108400141028     C                                          :PiVal
108500141028     C                                          :PiNum
108600141028     C                                          :PiInt)
108700141028     C*
108800141028     C                   ENDSR
108900141028     C***
109000141028
109100141028
109200141028     C*----------------------------------------------------*
109300141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
109400141028     C*----------------------------------------------------*
109500141028     C     SR_SPLIT      BEGSR
109600141028     C*
109700141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
109800141028     C                   CALL      'XSPLIT2'
109900141028     C                   PARM                    InStringa
110000141028     C                   PARM                    InSepara
110100141028     C                   PARM      '5'           InTypeOut
110200141028     C                   PARM      ''            wSkParam
110300141028     C                   PARM                    OutErrore
110400141028     C                   PARM                    OutMsg
110500141028     C                   MOVEA     wSkParam      SkSplitCSV_5
110600141028     C*
110700141028     C                   ENDSR
110800141028
110900141028
111000141028     C*----------------------------------------------------*
111100141028     C*  SCRITTURA BUFFER EDIVAT
111200141028     C*----------------------------------------------------*
111300141028     C     WRIVAT        BEGSR
111400141028     C*
111500141028     C                   IF        VATNOT <> *blanks
111600141028     C                   EVAL      VATFGS = VABFGS
111700141028     C                   EVAL      VATCCM = VABCCM
111800141028     C                   EVAL      VATLNP = VABLNP
111900141028     C                   EVAL      VATAAS = VABAAS
112000141028     C                   EVAL      VATNRS = VABNRS
112100141028     C                   EVAL      VATNSP = VABNSP
112200141028     C                   EVAL      VATCMR = VABCMR
112300141028     C                   EVAL      VATCNT = VABCNT
112400141028     C                   ADD       1             §CTROKVT
112500141028     C                   WRITE     EDIVAT00
112600141028     C                   ENDIF
112700141028     C*
112800141028     C                   ENDSR
112900000801
113000011113
113100011113     C*----------------------------------------------------*
113200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
113300011113     C*----------------------------------------------------*
113400011113     C     CHKIMPDIV     BEGSR
113500011113     C*
113600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
113700011113     C                   Z-ADD     *zeros        wrkDec            9 9
113800011113     C*
113900011113     C* Come prima cosa effettuo considerazioni sulla divisa
114000011113     C                   IF        vabIAS > *zeros
114100011113     C                   IF        vabVAS <> 'EUR'
114200011113     C                   EVAL      vabVAS =  'ITL'
114300011113     C                   ENDIF
114400011113     C                   ENDIF
114500011113     C*
114600011113     C                   IF        vabCAS > *zeros
114700011113     C                   IF        vabVCA <> 'EUR'
114800011113     C                   EVAL      vabVCA =  'ITL'
114900011113     C                   ENDIF
115000011113     C                   ENDIF
115100011113     C*
115200011113     C                   IF        vabVMD > *zeros
115300020305     C                   IF        vabVAD <> 'EUR'
115400011113     C                   EVAL      vabVAD =  'ITL'
115500011113     C                   ENDIF
115600011113     C                   ENDIF
115700011113     C*
115800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
115900011113     C                   Z-ADD     vabIAS        wrkDec
116000011113     C                   IF        wrkDec > *zeros
116100011113     C                   IF        vabVAS = 'ITL'
116200011113     C                   EVAL      vabIAS = *zeros
116300011113     C                   ENDIF
116400011113     C                   ENDIF
116500011113     C*
116600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
116700011113     C                   Z-ADD     vabCAS        wrkDec
116800011113     C                   IF        wrkDec > *zeros
116900011113     C                   IF        vabVCA = 'ITL'
117000011113     C                   EVAL      vabCAS = *zeros
117100011113     C                   ENDIF
117200011113     C                   ENDIF
117300011113     C*
117400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
117500011113     C                   Z-ADD     vabVMD        wrkDec
117600011113     C                   IF        wrkDec > *zeros
117700011113     C                   IF        vabVAD = 'ITL'
117800011113     C                   EVAL      vabVMD = *zeros
117900011113     C                   ENDIF
118000011113     C                   ENDIF
118100011113     C*
118200011113     C                   ENDSR
118300011113     C***
118400011113
118500011113
118600000801
118700000801
118800990920      /TITLE Invio dei dati al punto operativo.
118900010202     C     invio         BEGSR
119000990920     C*
119100140117     C* 1° invio EDIVAT
119200010201     C                   reset                   dscmz
119300010201     C                   move      vlrpoi        cmzdst
119400140117     C                   eval      cmzfld = 'EDIVATWR'
119500010201     C                   eval      cmzmbd = vlrhdl
119600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
119700021009     C***                if        prmfir = *blanks
119800140117     C                   eval      cmzfla = 'EDIVAT0F'
119900140117     C                   eval      cmzmba = 'EDIVAT0F'
120000021009     C***                else
120100021009     C***                eval      cmzfla = prmfir
120200021009     C***                eval      cmzmba = prmfir
120300021009     C***                endif
120400010201     C                   eval      cmznrr = *zeros
120500020305     C                   move      §ctrokvt      cmznrr
120600021018     C                   eval      cmzlba = vlrfl1
120700010201     C                   call(e)   'TIS711C'
120800010201     C                   parm                    dscmz
120900010201     C                   parm      *blanks       esito
121000010205     C                   if        %error
121100010205     C                             or cmzerr = '1'
121200010205     C                             or esito  = '1'
121300010205     C                   eval      wrkesito = '3'
121400010205     C                   else
121500010201     C*
121600140117     C* 2° invio EDIVAB
121700010201     C                   reset                   dscmz
121800010201     C                   move      vlrpoi        cmzdst
121900010201     C                   eval      cmzfld = vlrfou
122000010201     C                   eval      cmzmbd = vlrhdl
122100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
122200021009     C***                if        prmfir = *blanks
122300140117     C                   eval      cmzfla = 'EDIVAB0F'
122400140117     C                   eval      cmzmba = 'EDIVAB0F'
122500021009     C***                else
122600021009     C***                eval      cmzfla = prmfir
122700021009     C***                eval      cmzmba = prmfir
122800021009     C***                endif
122900010201     C                   eval      cmznrr = *zeros
123000010201     C                   move      §ctrokvb      cmznrr
123100021018     C                   eval      cmzlba = vlrfl1
123200010201     C                   call(e)   'TIS711C'
123300010201     C                   parm                    dscmz
123400010201     C                   parm      *blanks       esito
123500010201     C                   if        %error
123600010201     C                             or cmzerr = '1'
123700010201     C                             or esito  = '1'
123800010201     C                   eval      wrkesito = '3'
123900010201     C                   endif
124000010205     C                   endif
124100990920     C*
124200000613     C                   ENDSR
124300000613     C***
124400070411
124500141028
124600070411     C     *pssr         BEGSR
124700070411     C*
124800070411     C                   if        %open(tivin00r)
124900070411     C                   close     tivin00r
125000070411     C                   endif
125100140117     C                   if        %open(edivabwr)
125200140117     C                   close     edivabwr
125300070411     C                   endif
125400140117     C                   if        %open(edivatwr)
125500140117     C                   close     edivatwr
125600070411     C                   endif
125700070411     C*
125800070411     C* Effettuo la chiamata al CLLE preposto
125900140117     C                   call(e)   'TITVEVTC'
126000070411     C                   parm                    parccm
126100070411     C                   parm                    parmbr
126200070411     C                   parm      '2'           paropz
126300070411     C*
126400070411     C                   eval      wrkesito = '2'
126500070411     C*
126600070411     C                   seton                                        LR
126700070411     C*
126800070411     C                   ENDSR     '*CANCL'
126900070411     C***
127000161018
127100161018
127200161018     C*--------------------------------------------------------
127300161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
127400161018     C*--------------------------------------------------------
127500161018     C     CARTAB        BEGSR
127600161018     C*
127700161018     C* TABELLA '15' - NAZIONI
127800161018     C                   clear                   skNAZISO
127900161018     C                   clear                   skNAZBAR
128000161018     C                   eval      tblKUT = 1
128100161018     C                   eval      tblCOD = '15'
128200161018     C     KEYtabP       setll     tabel00f
128300161018     C     KEYtabP       reade     tabel00f
128400161018     C                   dow       not %eof(tabel00f)
128500161018     C                   if        tblFLG = *blanks
128600161018     C                   movel(p)  tblUNI        ds15
128700161018     C                   add       1             jNAZ
128800161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
128900161018     C                   eval      skNAZISO(jNAZ) = §15COD
129000161018     C                   endif
129100161018     C     KEYtabP       reade     tabel00f
129200161018     C                   enddo
129300161018     C*
129400161018     C                   ENDSR
129500161018     C***
129600070411
129700990910
129800000613     C     *inzsr        BEGSR
129900990910     C*
130000990910     C     *entry        plist
130100990920     C                   parm                    tivlrds
130200990921     C                   parm      wrkesito      esito
130300000724     C                   parm                    prmlit
130400000710     C                   parm                    prmfir
130500000613     C*
130600000830     C* CALCOLA LA DATA CORRENTE
130700140117     C                   time                    wn14             14 0
130800140117     C                   movel     wn14          oracor            6 0          *ORA
130900100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
131000100722     C                   eval      datcor = %dec(%date() : *ISO)
131100161018     C*
131200161018     C* Chiave su TABEL00F - parziale
131300161018     C     KEYtabP       KLIST
131400161018     C                   KFLD                    tblKUT
131500161018     C                   KFLD                    tblCOD
131600000830     C*
131700000613     C                   ENDSR
131800000613     C***
