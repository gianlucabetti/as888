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
011400170731     C*
011500170731     C                   if        *in53
011600170731     C                   call      'TIS7P2SER'
011700170731     C                   parm      'C'           pIn_Opz           1
011800170731     C                   parm                    tivlrds
011900170731     C                   parm                    EDIVABDS
012000170731     C                   parm      *blanks       pIn_CdIdAz        2
012100170731     C                   parm      *blanks       pIn_MaskPDF      50
012200170731     C                   parm      *blanks       pOut_Esito        1
012300170731     C                   endif
012400000616     C*
012500010201     C                   seton                                        LR
012600990908
012700000801
012800910830     C*--------------------------------------------------------
012900140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
013000910830     C*--------------------------------------------------------
013100040526     C     RWFILE        BEGSR
013200990910     C*
013300990914     C                   if        not %open(tivin00r)
013400990908     C                   open      tivin00r
013500990914     C                   endif
013600140117     C                   if        not %open(edivabwr)
013700140117     C                   open      edivabwr
013800990914     C                   endif
013900141028     C*
014000140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014100020305     C                   exsr      prevat
014200010201     C*
014300010202     C                   if        chkcall = '0'
014400010202     C*
014500140117     C                   if        not %open(edivatwr)
014600140117     C                   open      edivatwr
014700010201     C                   endif
014800990910     C*
014900010201     C                   clear                   §CTROKVB          5 0
015000020305     C                   clear                   §CTROKVT          5 0
015100000801     C                   clear                   §CTRMO            5 0
015200000801     C                   clear                   §CTRNO            5 0
015300141028     C*
015400141028     C* Inizializzazioni fuori ciclo
015500141028     C                   EXSR      INZVAR
015600140117     C*
015700140902     C                   if        *in53
015800140117     C                   call      'TIS7P2SER'
015900140117     C                   parm      'O'           pIn_Opz           1
016000140117     C                   parm                    tivlrds
016100140117     C                   parm                    EDIVABDS
016200140117     C                   parm      *blanks       pIn_CdIdAz        2
016300140117     C                   parm      *blanks       pIn_MaskPDF      50
016400140117     C                   parm      *blanks       pOut_Esito        1
016500140902     C                   endif
016600130708     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000040910     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300000801     C                   if        *in70 = *off
017400000801     C                             and
017500000801     C                             (vinflg = *blanks
017600000801     C                              or vinflg = '0'
017700000801     C                              or vinflg = '2')
017800000801     C*
017900000801     C                   clear                   vinmsg
018000141028     C*
018100141028     C* "normalizzo" il dato di input
018200141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
018300141028     C                                             %len(%trim(vindta))-1))
018400160209     C*
018500160209     C* Elimino le "duple di controllo EDIFACT"
018600160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
018700160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018800160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018900160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
019000160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
019100040910     C*
019200040910     C* Eseguo routine d traduzione
019300040910     C                   exsr      impvabvat
019400040802     C*
019500010305     C                   endif
019600000905     C*
019700000905     C                   else
019800000905     C                   eval      vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100000905     C  N70              update    tivin000
020200000905     C*
020300991022     C  N70              ENDdo
020400100722     C*
020500100722     C* Scarico i buffer testata ancora "in canna"
020600140922     C*
020700141028     C***  N31              ADD       1             §CTROKVB
020800141028     C***  N31              exsr      CHKWRI
020900141028     C***  N31              WRITE     EDIVAB00
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
062400150526     C* ...tipo record 'CNI' (rottuta di codice spedizione)
062500150526     C                   EVAL      wTag = 'CNI'
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
066700141028     C*
066800141028     C***
066900150526     C* ...tipo record 'RFF+FF' (riferimento spedizione)
067000170630     C***                EVAL      wTag = 'RFF+FF'
067100170630     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067200170630     C***                EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
067300170630     C***                EXSR      CHKNUM
067400170630     C***                IF        PiInt=*on
067500170630     C***                Z-ADD     PiVal         VABRMN
067600170630     C***                ELSE
067700170630     C***                EVAL      VABRMN = 1
067800170630     C***                Z-ADD     1             wRecErr
067900170630     C***                ENDIF
068000170630     C***                ENDIF
068100170630     C*
068200170630     C***
068300170630     C* ...tipo record 'CNI+1' (riferimento spedizione)
068400170630     C                   EVAL      wTag = 'CNI+1'
068500170630     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068600170630     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
068700170630     C                   EXSR      CHKNUM
068800170630     C                   IF        PiInt=*on
068900170630     C                   Z-ADD     PiVal         VABRMN
069000170630     C                   ELSE
069100170630     C                   EVAL      VABRMN = 1
069200170630     C                   Z-ADD     1             wRecErr
069300170630     C                   ENDIF
069400170630     C                   ENDIF
069500150526     C*
069600150526     C***
069700150526     C* ...tipo record 'RFF+CR' (riferimento spedizione)
069800161019     C***                EVAL      wTag = 'RFF+CR'
069900161019     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070000161019     C***                EVAL      VABRMA =
070100161019     C***                             %subst(wvindta:%len(%trim(wTag))+2)
070200161019     C***                ENDIF
070300161019     C*
070400161019     C***
070500161027     C* ...tipo record 'CNI' (riferimento spedizione)
070600161027     C                   EVAL      wTag = 'CNI'
070700161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070800161027     C                   EVAL      InStringa =
070900161027     C                                %subst(wvindta:%len(%trim(wTag))+2)
071000161027     C                   EVAL      InSepara = '+'
071100161027     C                   EXSR      SR_SPLIT
071200161027     C                   EVAL      InStringa =%trim(SkSplitCSV_5(2))
071300161019     C                   EVAL      InSepara = ','
071400161019     C                   EXSR      SR_SPLIT
071500161019     C                   EVAL      VABRMA = '0'+%trim(SkSplitCSV_5(2))+
071600161019     C                                          %trim(SkSplitCSV_5(3))+
071700161019     C                                          %trim(SkSplitCSV_5(4))
071800161019     C                   ENDIF
071900150527     C*
072000150527     C***
072100150527     C* ...tipo record 'DTM+2' (data consegna richiesta)
072200150527     C                   EVAL      wTag = 'DTM+2'
072300150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072400150527     C                   EVAL      PiStr=%subst(
072500150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
072600150527     C                   EXSR      CHKNUM
072700150527     C                   IF        PiInt=*on
072800150527     C                   Z-ADD     PiVal         VABDCR
072900150527     C                   ELSE
073000150527     C                   Z-ADD     1             wRecErr
073100150527     C                   ENDIF
073200150527     C                   ENDIF
073300141028     C*
073400141028     C***
073500141028     C* ...tipo record 'NAD+CN' (destinatario)
073600141028     C                   EVAL      wTag = 'NAD+CN'
073700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073800141028     C                   EVAL      InStringa =
073900141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
074000141028     C                   EVAL      InSepara = '+'
074100141028     C                   EXSR      SR_SPLIT
074200141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
074300141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
074400141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
074500141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
074600141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
074700141028     C                   IF        VABNZD = 'I'   OR
074800141028     C                             VABNZD = 'IT'  OR
074900141028     C                             VABNZD = 'ITA'
075000141028     C                   EVAL      VABNZD = *blanks
075100141028     C                   ENDIF
075200141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
075300141028     C                   EVAL      InSepara = ':'
075400141028     C                   EXSR      SR_SPLIT
075500141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
075600141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
075700141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
075800141028     C                                      %trim(SkSplitCSV_5(4))+
075900141028     C                                      %trim(SkSplitCSV_5(5))
076000141028     C                   ENDIF
076100150609     C*
076200150609     C***
076300150609     C* ...tipo record 'NAD+CZ' (mittente originale)
076400150609     C                   EVAL      wTag = 'NAD+CZ'
076500150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076600150609     C                   EVAL      InStringa =
076700150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
076800150609     C                   EVAL      InSepara = '+'
076900150609     C                   EXSR      SR_SPLIT
077000150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
077100150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
077200150609     C                   IF        VABNMO = 'I'   OR
077300150609     C                             VABNMO = 'IT'  OR
077400150609     C                             VABNMO = 'ITA'
077500150609     C                   EVAL      VABNMO = *blanks
077600161018     C                   ELSE
077700161018     C                   Z-ADD     1             jNAZ
077800161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
077900161018     C                   IF        %found
078000161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
078100161018     C                   ENDIF
078200161018     C                   ENDIF
078300161018     C*
078400150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
078500150609     C                   EVAL      InSepara = ':'
078600150609     C                   EXSR      SR_SPLIT
078700150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
078800150609     C                   ENDIF
078900141104     C*
079000141104     C***
079100141104     C* ...tipo record 'NAD+UC' (destinatario)
079200141104     C                   EVAL      wTag = 'NAD+UC'
079300141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079400141104     C                   EVAL      InStringa =
079500141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
079600141104     C                   EVAL      InSepara = '+'
079700141104     C                   EXSR      SR_SPLIT
079800141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
079900141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
080000141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
080100141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
080200141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
080300141104     C                   IF        VABNZD = 'I'   OR
080400141104     C                             VABNZD = 'IT'  OR
080500141104     C                             VABNZD = 'ITA'
080600141104     C                   EVAL      VABNZD = *blanks
080700141104     C                   ENDIF
080800141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
080900141104     C                   EVAL      InSepara = ':'
081000141104     C                   EXSR      SR_SPLIT
081100141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
081200141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
081300141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
081400141104     C                                      %trim(SkSplitCSV_5(4))+
081500141104     C                                      %trim(SkSplitCSV_5(5))
081600141104     C                   ENDIF
081700141028     C*
081800141028     C***
081900141028     C* ...tipo record 'GID' (colli)
082000150609     C                   EVAL      wTag = 'GID+'
082100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082200141028     C                   EVAL      InStringa =
082300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
082400141028     C                   EVAL      InSepara = '+'
082500141028     C                   EXSR      SR_SPLIT
082600150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
082700150526     C                   EVAL      InSepara = ':'
082800150526     C                   EXSR      SR_SPLIT
082900150526     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
083000141028     C                   EXSR      CHKNUM
083100141028     C                   IF        PiInt=*on
083200150526     C                   ADD       PiVal         VABNCL
083300141028     C                   ELSE
083400141028     C                   Z-ADD     1             wRecErr
083500141028     C                   ENDIF
083600141028     C                   ENDIF
083700150527     C*
083800150527     C***
083900150527     C* ...tipo record 'MOA+AAA' (contrassegno / divisa)
084000150527     C                   EVAL      wTag = 'MOA+AAA'
084100150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
084200150527     C                   EVAL      wFlgCAS = '1'
084300150527     C                   EVAL      InStringa =
084400150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
084500150527     C                   EVAL      InSepara = ':'
084600150527     C                   EXSR      SR_SPLIT
084700150527     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
084800150527     C                   EXSR      CHKNUM
084900150527     C                   IF        PiNum=*on
085000150527     C                   ADD       PiVal         VABCAS
085100150527     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
085200150527     C                   ELSE
085300150527     C                   Z-ADD     1             wRecErr
085400150527     C                   ENDIF
085500150527     C                   ENDIF
085600141028     C*
085700141028     C***
085800141028     C* ...tipo record 'FTX+AAA' (natura merce)
085900141028     C                   EVAL      wTag = 'FTX+AAA'
086000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086100141028     C                   EVAL      InStringa =
086200141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
086300141028     C                   EVAL      InSepara = '+'
086400141028     C                   EXSR      SR_SPLIT
086500141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
086600141028     C                   ENDIF
086700150622     C*
086800150622     C***
086900150622     C* ...tipo record 'TSR+' (tipo servizio)
087000150622     C                   EVAL      wTag = 'TSR+'
087100150622     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087200150622     C                   EVAL      InStringa =
087300150622     C                                %subst(wvindta:%len(%trim(wTag))+2)
087400150622     C                   EVAL      InSepara = '+'
087500150622     C                   EXSR      SR_SPLIT
087600150622     C                   SELECT
087700150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = '3'
087800150622     C                   EVAL      VABTSP = 'C'
087900150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'E'
088000150622     C                   EVAL      VABTSP = 'E'
088100150622     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'H'
088200150622     C                   EVAL      VABTSP = 'H'
088300150622     C                   ENDSL
088400150622     C                   ENDIF
088500150527     C*
088600150527     C***
088700150527     C* ...tipo record 'FTX+DIN' (note di consegna)
088800170316     C***                EVAL      wTag = 'FTX+DIN'
088900170316     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089000170316     C***                EVAL      InStringa =
089100170316     C***                             %subst(wvindta:%len(%trim(wTag))+2)
089200170316     C***                EVAL      InSepara = '+'
089300170316     C***                EXSR      SR_SPLIT
089400170316     C***                EVAL      wNOTE = %trim(wNOTE) + ' ' +
089500170316     C***                                  %trim(SkSplitCSV_5(3))
089600170316     C***                ENDIF
089700161013     C*
089800161013     C***
089900161013     C* ...tipo record 'COM' (email destinatario ai fini alert)
090000161013     C                   EVAL      wTag = 'COM'
090100161013     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090200161013     C                   EVAL      InStringa =
090300161013     C                                %subst(wvindta:%len(%trim(wTag))+2)
090400161013     C                   EVAL      InSepara = ':'
090500161013     C                   EXSR      SR_SPLIT
090600161013     C                   IF        SkSplitCSV_5(2) = 'EM'
090700161013     C                   EVAL      wEMAIL = SkSplitCSV_5(1)
090800161013     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
090900161013     C                   EVAL      VATTRC = 'I'
091000161013     C                   EXSR      WRIVAT
091100161013     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
091200161013     C                   EVAL      VATTRC = 'J'
091300161013     C                   EXSR      WRIVAT
091400161013     C                   ENDIF
091500161013     C                   ENDIF
091600150527     C*
091700150527     C***
091800150527     C* ...tipo record 'FTX+ACB' (note aggiuntive)
091900150527     C                   EVAL      wTag = 'FTX+ACB'
092000150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092100150527     C                   EVAL      InStringa =
092200150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
092300150527     C                   EVAL      InSepara = '+'
092400150527     C                   EXSR      SR_SPLIT
092500150527     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
092600150527     C                                     %trim(SkSplitCSV_5(3))
092700150527     C                   ENDIF
092800150609     C*
092900150609     C***
093000170316     C* ...tipo record 'FTX+DIN' (telefono destinatario / note / appuntamento / etc...)
093100170316     C                   EVAL      wTag = 'FTX+DIN'
093200150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093300150609     C                   EVAL      InStringa =
093400150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
093500150609     C                   EVAL      InSepara = '+'
093600150609     C                   EXSR      SR_SPLIT
093700150609     C                   MOVEL     *blanks       wSplitCSV_5     256
093800150609     C                   EVAL      wSplitCSV_5 = SkSplitCSV_5(3)
093900150609     C     minu:maiu     XLATE     wSplitCSV_5   wSplitCSV_5                    *Minus -> Maiuscolo
094000160513     C                   SELECT
094100160513     C*
094200160513     C                   WHEN      %scan('PHONE':wSplitCSV_5) > *zeros
094300150609     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
094400150609     C                                            %scan('PHONE':wSplitCSV_5)+
094500150609     C                                            %len('PHONE')+1))
094600150609     C                   EVAL      VATTRC = 'B'
094700150609     C                   EXSR      WRIVAT
094800160513     C*
094900160513     C                   WHEN      %scan('TELF':wSplitCSV_5) > *zeros
095000160513     C                   EVAL      VATNOT = %trim(%subst(wSplitCSV_5:
095100160513     C                                            %scan('TELF':wSplitCSV_5)+
095200160513     C                                            %len('TELF')+1))
095300160513     C                   EVAL      VATTRC = 'B'
095400160513     C                   EXSR      WRIVAT
095500160513     C*
095600160513     C                   WHEN      %scan('CHIAMARE PRIMA DELLA CONSEGNA':
095700160513     C                                   wSplitCSV_5) > *zeros
095800160513     C                   EVAL      VABTC1 = 'A'
095900160513     C*
096000160513     C                   WHEN      %scan('CONSEGNA TASSATIVA':
096100160513     C                                   wSplitCSV_5) > *zeros
096200160513     C                   Z-ADD     *zeros        wPos              3 0
096300160513     C                   MOVEL     *blanks       wDCR             10
096400160513     C                   EVAL      wPos = %scan('CONSEGNA TASSATIVA':
096500160513     C                                          wSplitCSV_5)
096600160513     C                   EVAL      wDCR   = %trim(%subst(wSplitCSV_5:wPos+1))
096700160513     C                   EVAL      PiStr=%subst(wDCR:7:4)+
096800160513     C                                   %subst(wDCR:4:2)+
096900160513     C                                   %subst(wDCR:1:2)
097000160513     C                   EXSR      CHKNUM
097100160513     C                   IF        PiNum=*on
097200160513     C                   ADD       PiVal         VABDCR
097300160513     C                   ELSE
097400160513     C                   Z-ADD     1             wRecErr
097500160513     C                   ENDIF
097600160513     C*
097700160513     C                   OTHER
097800150609     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
097900150609     C                                     %trim(SkSplitCSV_5(3))
098000160513     C                   ENDSL
098100150609     C                   ENDIF
098200141028     C*
098300141028     C***
098400161013     C* ...tipo record 'MEA+WT+AAD' (peso spedizione)
098500161013     C                   EVAL      wTag = 'MEA+WT+AAD'
098600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
098700141028     C                   EVAL      InStringa =
098800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
098900141028     C                   EVAL      InSepara = ':'
099000141028     C                   EXSR      SR_SPLIT
099100141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
099200141028     C                   EXSR      CHKNUM
099300141028     C                   IF        PiNum=*on
099400150924     C                   ADD       PiVal         VABPKB
099500141028     C                   ELSE
099600141028     C                   Z-ADD     1             wRecErr
099700141028     C                   ENDIF
099800141028     C                   ENDIF
099900141028     C*
100000141028     C***
100100141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
100200141028     C                   EVAL      wTag = 'MEA+VOL'
100300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100400141028     C                   EVAL      InStringa =
100500141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
100600141028     C                   EVAL      InSepara = ':'
100700141028     C                   EXSR      SR_SPLIT
100800141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
100900141028     C                   EXSR      CHKNUM
101000141028     C                   IF        PiNum=*on
101100150924     C                   ADD       PiVal         VABVLB
101200141028     C                   ELSE
101300141028     C                   Z-ADD     1             wRecErr
101400141028     C                   ENDIF
101500141028     C                   ENDIF
101600141028     C*
101700141028     C***
101800170316     C* ...tipo record 'GIN+001' (dettagli segnacolli)
101900170316     C                   EVAL      wTag = 'GIN+001'
102000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102100150526     C                   EVAL      InStringa =
102200150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
102300161014     C     '+':':'       XLATE     InStringa     InStringa
102400161013     C                   EVAL      InSepara = ':'
102500150526     C                   EXSR      SR_SPLIT
102600150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
102700150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
102800141028     C                   EVAL      VATTRC = 'E'
102900141028     C                   EXSR      WRIVAT
103000150526     C                   ENDIF
103100150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
103200150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
103300150526     C                   EVAL      VATTRC = 'E'
103400150526     C                   EXSR      WRIVAT
103500150526     C                   ENDIF
103600150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
103700150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
103800150526     C                   EVAL      VATTRC = 'E'
103900150526     C                   EXSR      WRIVAT
104000150526     C                   ENDIF
104100150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
104200150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
104300150526     C                   EVAL      VATTRC = 'E'
104400150526     C                   EXSR      WRIVAT
104500150526     C                   ENDIF
104600150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
104700150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
104800150526     C                   EVAL      VATTRC = 'E'
104900150526     C                   EXSR      WRIVAT
105000150526     C                   ENDIF
105100161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
105200161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(6))
105300161017     C                   EVAL      VATTRC = 'E'
105400161017     C                   EXSR      WRIVAT
105500161017     C                   ENDIF
105600161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
105700161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(7))
105800161017     C                   EVAL      VATTRC = 'E'
105900161017     C                   EXSR      WRIVAT
106000161017     C                   ENDIF
106100161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
106200161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(8))
106300161017     C                   EVAL      VATTRC = 'E'
106400161017     C                   EXSR      WRIVAT
106500161017     C                   ENDIF
106600161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
106700161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(9))
106800161017     C                   EVAL      VATTRC = 'E'
106900161017     C                   EXSR      WRIVAT
107000161017     C                   ENDIF
107100161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
107200161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(10))
107300161017     C                   EVAL      VATTRC = 'E'
107400161017     C                   EXSR      WRIVAT
107500161017     C                   ENDIF
107600141028     C                   ENDIF
107700141028     C*
107800141028     C***
107900141028     C* ...tipo record '???' (PDF per packing-list)
108000141028     C                   EVAL      wTag = '???PDF'
108100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
108200141028     C*
108300141028     C* ......VATNOT_P
108400141028     C                   if        *in53
108500141028     C*
108600141028     C* Solo se valorizzato RMN
108700141028     C                   if        VABRMA <> *blanks
108800141028     C                   eval      pIn_MaskPDF =
108900141028     C                                     %trim(%subst(wvindta:1292:100))
109000141028     C                   call      'TIS7P2SER'
109100141028     C                   parm      'W'           pIn_Opz           1
109200141028     C                   parm                    tivlrds
109300141028     C                   parm                    EDIVABDS
109400141028     C                   parm      '10'          pIn_CdIdAz        2
109500141028     C                   parm                    pIn_MaskPDF      50
109600141028     C                   parm      *blanks       pOut_Esito        1
109700141028     C                   endif
109800141028     C                   endif
109900141028     C*
110000141028     C                   ENDIF
110100141028     C*
110200141028     C***
110300141028     C* ...tipo record 'UNT' (fine bolla)
110400141028     C                   EVAL      wTag = 'UNT'
110500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
110600150526     C                   IF        *IN90
110700141028     C  N31              ADD       1             §CTROKVB
110800141028     C  N31              exsr      CHKWRI
110900140117     C  N31              WRITE     EDIVAB00
111000150526     C                   ENDIF
111100150526     C                   ENDIF
111200010202     C*
111300000801     C* Ebbene...
111400000801     C                   ADD       1             §CTRMO
111500141028     C                   IF        wRecErr <> *zeros
111600000801     C                   ADD       1             §CTRNO
111700000801     C                   EVAL      vinflg = '2'
111800000801     C                   ENDIF
111900000801     C*
112000000801     C                   ENDSR
112100010202     C*----------------------------------------------------*
112200140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
112300010202     C*----------------------------------------------------*
112400020305     C     PREVAT        BEGSR
112500010202     C*
112600140117     C* Compongo il nome del membro da dare al EDIVATWR
112700010202     C                   eval      parmbr = vlrhdl
112800010202     C                   movel     'M'           parmbr
112900060113     C                   eval      parccm = vlrksc
113000010202     C                   eval      paropz = '1'
113100010202     C* Effettuo la chiamata al CLLE preposto
113200140117     C                   call(e)   'TITVEVTC'
113300010202     C                   parm                    parccm
113400010202     C                   parm                    parmbr
113500010202     C                   parm                    paropz
113600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
113700010202     C                   if        %error
113800010202     C                   movel     '1'           chkcall
113900010202     C                   else
114000010202     C                   movel     '0'           chkcall
114100010202     C                   endif
114200010202     C*
114300010202     C                   ENDSR
114400141028     C*----------------------------------------------------*
114500141028     C*  CONTROLLO NUMERICITA' CAMPI
114600141028     C*----------------------------------------------------*
114700141028     C     CHKNUM        BEGSR
114800141028     C*
114900141028     C                   IF        PiDecChr = *blanks
115000141028     C                   EVAL      PiDecChr = CharNUM
115100141028     C                   ENDIF
115200141028     C*
115300141028     C                   callp     UBISNUM_Check(PiStr
115400141028     C                                          :PiDecChr
115500141028     C                                          :PiVal
115600141028     C                                          :PiNum
115700141028     C                                          :PiInt)
115800141028     C*
115900141028     C                   ENDSR
116000141028     C***
116100141028
116200141028
116300141028     C*----------------------------------------------------*
116400141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
116500141028     C*----------------------------------------------------*
116600141028     C     SR_SPLIT      BEGSR
116700141028     C*
116800141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
116900141028     C                   CALL      'XSPLIT2'
117000141028     C                   PARM                    InStringa
117100141028     C                   PARM                    InSepara
117200141028     C                   PARM      '5'           InTypeOut
117300141028     C                   PARM      ''            wSkParam
117400141028     C                   PARM                    OutErrore
117500141028     C                   PARM                    OutMsg
117600141028     C                   MOVEA     wSkParam      SkSplitCSV_5
117700141028     C*
117800141028     C                   ENDSR
117900141028
118000141028
118100141028     C*----------------------------------------------------*
118200141028     C*  SCRITTURA BUFFER EDIVAT
118300141028     C*----------------------------------------------------*
118400141028     C     WRIVAT        BEGSR
118500141028     C*
118600141028     C                   IF        VATNOT <> *blanks
118700141028     C                   EVAL      VATFGS = VABFGS
118800141028     C                   EVAL      VATCCM = VABCCM
118900141028     C                   EVAL      VATLNP = VABLNP
119000141028     C                   EVAL      VATAAS = VABAAS
119100141028     C                   EVAL      VATNRS = VABNRS
119200141028     C                   EVAL      VATNSP = VABNSP
119300141028     C                   EVAL      VATCMR = VABCMR
119400141028     C                   EVAL      VATCNT = VABCNT
119500141028     C                   ADD       1             §CTROKVT
119600141028     C                   WRITE     EDIVAT00
119700141028     C                   ENDIF
119800141028     C*
119900141028     C                   ENDSR
120000000801
120100011113
120200011113     C*----------------------------------------------------*
120300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
120400011113     C*----------------------------------------------------*
120500011113     C     CHKIMPDIV     BEGSR
120600011113     C*
120700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
120800011113     C                   Z-ADD     *zeros        wrkDec            9 9
120900011113     C*
121000011113     C* Come prima cosa effettuo considerazioni sulla divisa
121100011113     C                   IF        vabIAS > *zeros
121200011113     C                   IF        vabVAS <> 'EUR'
121300011113     C                   EVAL      vabVAS =  'ITL'
121400011113     C                   ENDIF
121500011113     C                   ENDIF
121600011113     C*
121700011113     C                   IF        vabCAS > *zeros
121800011113     C                   IF        vabVCA <> 'EUR'
121900011113     C                   EVAL      vabVCA =  'ITL'
122000011113     C                   ENDIF
122100011113     C                   ENDIF
122200011113     C*
122300011113     C                   IF        vabVMD > *zeros
122400020305     C                   IF        vabVAD <> 'EUR'
122500011113     C                   EVAL      vabVAD =  'ITL'
122600011113     C                   ENDIF
122700011113     C                   ENDIF
122800011113     C*
122900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
123000011113     C                   Z-ADD     vabIAS        wrkDec
123100011113     C                   IF        wrkDec > *zeros
123200011113     C                   IF        vabVAS = 'ITL'
123300011113     C                   EVAL      vabIAS = *zeros
123400011113     C                   ENDIF
123500011113     C                   ENDIF
123600011113     C*
123700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
123800011113     C                   Z-ADD     vabCAS        wrkDec
123900011113     C                   IF        wrkDec > *zeros
124000011113     C                   IF        vabVCA = 'ITL'
124100011113     C                   EVAL      vabCAS = *zeros
124200011113     C                   ENDIF
124300011113     C                   ENDIF
124400011113     C*
124500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
124600011113     C                   Z-ADD     vabVMD        wrkDec
124700011113     C                   IF        wrkDec > *zeros
124800011113     C                   IF        vabVAD = 'ITL'
124900011113     C                   EVAL      vabVMD = *zeros
125000011113     C                   ENDIF
125100011113     C                   ENDIF
125200011113     C*
125300011113     C                   ENDSR
125400011113     C***
125500011113
125600011113
125700000801
125800000801
125900990920      /TITLE Invio dei dati al punto operativo.
126000010202     C     invio         BEGSR
126100990920     C*
126200140117     C* 1° invio EDIVAT
126300010201     C                   reset                   dscmz
126400010201     C                   move      vlrpoi        cmzdst
126500140117     C                   eval      cmzfld = 'EDIVATWR'
126600010201     C                   eval      cmzmbd = vlrhdl
126700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
126800021009     C***                if        prmfir = *blanks
126900140117     C                   eval      cmzfla = 'EDIVAT0F'
127000140117     C                   eval      cmzmba = 'EDIVAT0F'
127100021009     C***                else
127200021009     C***                eval      cmzfla = prmfir
127300021009     C***                eval      cmzmba = prmfir
127400021009     C***                endif
127500010201     C                   eval      cmznrr = *zeros
127600020305     C                   move      §ctrokvt      cmznrr
127700021018     C                   eval      cmzlba = vlrfl1
127800010201     C                   call(e)   'TIS711C'
127900010201     C                   parm                    dscmz
128000010201     C                   parm      *blanks       esito
128100010205     C                   if        %error
128200010205     C                             or cmzerr = '1'
128300010205     C                             or esito  = '1'
128400010205     C                   eval      wrkesito = '3'
128500010205     C                   else
128600010201     C*
128700140117     C* 2° invio EDIVAB
128800010201     C                   reset                   dscmz
128900010201     C                   move      vlrpoi        cmzdst
129000010201     C                   eval      cmzfld = vlrfou
129100010201     C                   eval      cmzmbd = vlrhdl
129200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
129300021009     C***                if        prmfir = *blanks
129400140117     C                   eval      cmzfla = 'EDIVAB0F'
129500140117     C                   eval      cmzmba = 'EDIVAB0F'
129600021009     C***                else
129700021009     C***                eval      cmzfla = prmfir
129800021009     C***                eval      cmzmba = prmfir
129900021009     C***                endif
130000010201     C                   eval      cmznrr = *zeros
130100010201     C                   move      §ctrokvb      cmznrr
130200021018     C                   eval      cmzlba = vlrfl1
130300010201     C                   call(e)   'TIS711C'
130400010201     C                   parm                    dscmz
130500010201     C                   parm      *blanks       esito
130600010201     C                   if        %error
130700010201     C                             or cmzerr = '1'
130800010201     C                             or esito  = '1'
130900010201     C                   eval      wrkesito = '3'
131000010201     C                   endif
131100010205     C                   endif
131200990920     C*
131300000613     C                   ENDSR
131400000613     C***
131500070411
131600141028
131700070411     C     *pssr         BEGSR
131800070411     C*
131900070411     C                   if        %open(tivin00r)
132000070411     C                   close     tivin00r
132100070411     C                   endif
132200140117     C                   if        %open(edivabwr)
132300140117     C                   close     edivabwr
132400070411     C                   endif
132500140117     C                   if        %open(edivatwr)
132600140117     C                   close     edivatwr
132700070411     C                   endif
132800070411     C*
132900070411     C* Effettuo la chiamata al CLLE preposto
133000140117     C                   call(e)   'TITVEVTC'
133100070411     C                   parm                    parccm
133200070411     C                   parm                    parmbr
133300070411     C                   parm      '2'           paropz
133400070411     C*
133500070411     C                   eval      wrkesito = '2'
133600070411     C*
133700070411     C                   seton                                        LR
133800070411     C*
133900070411     C                   ENDSR     '*CANCL'
134000070411     C***
134100161018
134200161018
134300161018     C*--------------------------------------------------------
134400161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
134500161018     C*--------------------------------------------------------
134600161018     C     CARTAB        BEGSR
134700161018     C*
134800161018     C* TABELLA '15' - NAZIONI
134900161018     C                   clear                   skNAZISO
135000161018     C                   clear                   skNAZBAR
135100161018     C                   eval      tblKUT = 1
135200161018     C                   eval      tblCOD = '15'
135300161018     C     KEYtabP       setll     tabel00f
135400161018     C     KEYtabP       reade     tabel00f
135500161018     C                   dow       not %eof(tabel00f)
135600161018     C                   if        tblFLG = *blanks
135700161018     C                   movel(p)  tblUNI        ds15
135800161018     C                   add       1             jNAZ
135900161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
136000161018     C                   eval      skNAZISO(jNAZ) = §15COD
136100161018     C                   endif
136200161018     C     KEYtabP       reade     tabel00f
136300161018     C                   enddo
136400161018     C*
136500161018     C                   ENDSR
136600161018     C***
136700070411
136800990910
136900000613     C     *inzsr        BEGSR
137000990910     C*
137100990910     C     *entry        plist
137200990920     C                   parm                    tivlrds
137300990921     C                   parm      wrkesito      esito
137400000724     C                   parm                    prmlit
137500000710     C                   parm                    prmfir
137600000613     C*
137700000830     C* CALCOLA LA DATA CORRENTE
137800140117     C                   time                    wn14             14 0
137900140117     C                   movel     wn14          oracor            6 0          *ORA
138000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
138100100722     C                   eval      datcor = %dec(%date() : *ISO)
138200161018     C*
138300161018     C* Chiave su TABEL00F - parziale
138400161018     C     KEYtabP       KLIST
138500161018     C                   KFLD                    tblKUT
138600161018     C                   KFLD                    tblCOD
138700000830     C*
138800000613     C                   ENDSR
138900000613     C***
