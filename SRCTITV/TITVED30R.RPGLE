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
027600170411     C                   ELSE
027700170411     C                   EVAL      VABTIC = *blanks
027800140922     C                   ENDIF
027900140922     C*
028000140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
028100141028     C                   EXSR      CHKIMPDIV
028200150527     C*
028300150527     C* Scompongo le note
028400150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
028500150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
028600140922     C*
028700140922     C                   ENDSR
028800141028     C*----------------------------------------------------*
028900141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029000141028     C*----------------------------------------------------*
029100141028     C     INZVAR        BEGSR
029200141028     C*
029300141028     C* Inizializzo variabili di wrk
029400141028     C                   Z-ADD     *zeros        Num5_0            5 0
029500141028     C                   MOVEL     '0'           wFlgCAS
029600141028     C                   MOVEL     *blanks       wVATNOT_A
029700141028     C                   MOVEL     *blanks       wVATNOT_B
029800141028     C                   MOVEL     *blanks       wVATNOT_E
029900141028     C                   MOVEL     *blanks       wVATNOT_I
030000141028     C                   MOVEL     *blanks       wVATNOT_J
030100141028     C                   MOVEL     *blanks       wVATNOT_S
030200141028     C                   MOVEL     *blanks       wVATNOT_P
030300141028     C                   CLEAR                   SkSplitCSV_5
030400150527     C                   MOVEL     *blanks       wNOTE
030500141028     C*
030600141028     C* Reimposto i valori di default
030700141028     C                   EXSR      DEFCAM
030800141028     C*
030900141028     C                   ENDSR
031000141028     C*----------------------------------------------------*
031100141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
031200141028     C*----------------------------------------------------*
031300141028     C     DEFCAM        BEGSR
031400141028     C*
031500141028     C                   CLEAR                   EDIVAB00
031600141028     C                   CLEAR                   EDIVAT00
031700141028     C*
031800141028     C* Imposto i valori bolla di default
031900141028     C                   EVAL      VABCTR = 000
032000141028     C                   EVAL      VABCBO = '1'
032100141104     C                   EVAL      VABTSP = 'C'
032200141028     C*
032300141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032400141028     C* e delimitatore testo.
032500141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032600141028     C*
032700141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
032800141028     C                   EVAL      CharSOS = CharNUM
032900141028     C*
033000141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
033100141028     C                   SETOFF                                       53
033200141028     C                   SELECT
033300141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
033400141028     C                   SETON                                        53
033500141028     C                   ENDSL
033600141028     C*
033700141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
033800141028     C                   EVAL      posDaDft = 1
033900141028     C                   EVAL      posADft  = 0
034000141028     C                   EVAL      wGiroDft = 0
034100141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034200141028     C                             posDaDft > 0
034300141028     C*
034400141028     C* Gestisco il 1° giro
034500141028     C                   IF        wGiroDft = 0
034600141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
034700141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
034800141028     C* Incremento il contatore dei "giri"
034900141028     C                   EVAL      wGiroDft = 1
035000141028     C                   ELSE
035100141028     C                   EVAL      posDaDft = posADft
035200141028     C                   ENDIF
035300141028     C* Eseguo lo scan x trovare la fine del campo corrente
035400141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035500141028     C*
035600141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
035700141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
035800141028     C                   IF        posDaDft > 0 AND
035900141028     C                             posADft  > 0
036000141028     C* NCL
036100141028     C                   IF        %subst(
036200141028     C                             %subst(vlrppt:posDaDft+1:
036300141028     C                             posADft-posDaDft-1):1:3)
036400141028     C                             = 'NCL'
036500141028     C                   EVAL      PiStr=%trim(%subst(
036600141028     C                             %subst(vlrppt:posDaDft+1:
036700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036800141028     C                   EXSR      CHKNUM
036900141028     C                   IF        PiInt=*on
037000141028     C                   Z-ADD     PiVal         VABNCL
037100141028     C                   ENDIF
037200141028     C                   ENDIF
037300141028     C* CCM
037400141028     C                   IF        %subst(
037500141028     C                             %subst(vlrppt:posDaDft+1:
037600141028     C                             posADft-posDaDft-1):1:3)
037700141028     C                             = 'CCM'
037800141028     C                   EVAL      PiStr=%trim(%subst(
037900141028     C                             %subst(vlrppt:posDaDft+1:
038000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038100141028     C                   EXSR      CHKNUM
038200141028     C                   IF        PiInt=*on
038300141028     C                   Z-ADD     PiVal         VABCCM
038400141028     C                   ENDIF
038500141028     C                   ENDIF
038600141028     C* LNP
038700141028     C                   IF        %subst(
038800141028     C                             %subst(vlrppt:posDaDft+1:
038900141028     C                             posADft-posDaDft-1):1:3)
039000141028     C                             = 'LNP'
039100141028     C                   EVAL      PiStr=%trim(%subst(
039200141028     C                             %subst(vlrppt:posDaDft+1:
039300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039400141028     C                   EXSR      CHKNUM
039500141028     C                   IF        PiInt=*on
039600141028     C                   Z-ADD     PiVal         VABLNP
039700141028     C                   ENDIF
039800141028     C                   ENDIF
039900141028     C* NRS
040000141028     C                   IF        %subst(
040100141028     C                             %subst(vlrppt:posDaDft+1:
040200141028     C                             posADft-posDaDft-1):1:3)
040300141028     C                             = 'NRS'
040400141028     C                   EVAL      PiStr=%trim(%subst(
040500141028     C                             %subst(vlrppt:posDaDft+1:
040600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040700141028     C                   EXSR      CHKNUM
040800141028     C                   IF        PiInt=*on
040900141028     C                   Z-ADD     PiVal         VABNRS
041000141028     C                   ENDIF
041100141028     C                   ENDIF
041200141028     C* CTR
041300141028     C                   IF        %subst(
041400141028     C                             %subst(vlrppt:posDaDft+1:
041500141028     C                             posADft-posDaDft-1):1:3)
041600141028     C                             = 'CTR'
041700141028     C                   EVAL      PiStr=%trim(%subst(
041800141028     C                             %subst(vlrppt:posDaDft+1:
041900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042000141028     C                   EXSR      CHKNUM
042100141028     C                   IF        PiInt=*on
042200141028     C                   Z-ADD     PiVal         VABCTR
042300141028     C                   ENDIF
042400141028     C                   ENDIF
042500141028     C* PKB
042600141028     C                   IF        %subst(
042700141028     C                             %subst(vlrppt:posDaDft+1:
042800141028     C                             posADft-posDaDft-1):1:3)
042900141028     C                             = 'PKB'
043000141028     C                   EVAL      PiStr=%trim(%subst(
043100141028     C                             %subst(vlrppt:posDaDft+1:
043200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043300141028     C                   EXSR      CHKNUM
043400141028     C                   IF        PiNum=*on
043500141028     C                   Z-ADD     PiVal         VABPKB
043600141028     C                   ENDIF
043700141028     C                   ENDIF
043800141028     C* VLB
043900141028     C                   IF        %subst(
044000141028     C                             %subst(vlrppt:posDaDft+1:
044100141028     C                             posADft-posDaDft-1):1:3)
044200141028     C                             = 'VLB'
044300141028     C                   EVAL      PiStr=%trim(%subst(
044400141028     C                             %subst(vlrppt:posDaDft+1:
044500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044600141028     C                   EXSR      CHKNUM
044700141028     C                   IF        PiNum=*on
044800141028     C                   Z-ADD     PiVal         VABVLB
044900141028     C                   ENDIF
045000141028     C                   ENDIF
045100141028     C* QFT
045200141028     C                   IF        %subst(
045300141028     C                             %subst(vlrppt:posDaDft+1:
045400141028     C                             posADft-posDaDft-1):1:3)
045500141028     C                             = 'QFT'
045600141028     C                   EVAL      PiStr=%trim(%subst(
045700141028     C                             %subst(vlrppt:posDaDft+1:
045800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045900141028     C                   EXSR      CHKNUM
046000141028     C                   IF        PiNum=*on
046100141028     C                   Z-ADD     PiVal         VABQFT
046200141028     C                   ENDIF
046300141028     C                   ENDIF
046400141028     C* CBO
046500141028     C                   IF        %subst(
046600141028     C                             %subst(vlrppt:posDaDft+1:
046700141028     C                             posADft-posDaDft-1):1:3)
046800141028     C                             = 'CBO'
046900141028     C                   EVAL      VABCBO=%trim(%subst(
047000141028     C                             %subst(vlrppt:posDaDft+1:
047100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200141028     C                   ENDIF
047300141028     C* TSP
047400141028     C                   IF        %subst(
047500141028     C                             %subst(vlrppt:posDaDft+1:
047600141028     C                             posADft-posDaDft-1):1:3)
047700141028     C                             = 'TSP'
047800141028     C                   EVAL      VABTSP=%trim(%subst(
047900141028     C                             %subst(vlrppt:posDaDft+1:
048000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100141028     C                   ENDIF
048200141028     C* VAS
048300141028     C                   IF        %subst(
048400141028     C                             %subst(vlrppt:posDaDft+1:
048500141028     C                             posADft-posDaDft-1):1:3)
048600141028     C                             = 'VAS'
048700141028     C                   EVAL      VABVAS=%trim(%subst(
048800141028     C                             %subst(vlrppt:posDaDft+1:
048900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000141028     C                   ENDIF
049100141028     C* VCA
049200141028     C                   IF        %subst(
049300141028     C                             %subst(vlrppt:posDaDft+1:
049400141028     C                             posADft-posDaDft-1):1:3)
049500141028     C                             = 'VCA'
049600141028     C                   EVAL      VABVCA=%trim(%subst(
049700141028     C                             %subst(vlrppt:posDaDft+1:
049800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900141028     C                   ENDIF
050000141028     C* TIC
050100141028     C                   IF        %subst(
050200141028     C                             %subst(vlrppt:posDaDft+1:
050300141028     C                             posADft-posDaDft-1):1:3)
050400141028     C                             = 'TIC'
050500141028     C                   EVAL      VABTIC=%trim(%subst(
050600141028     C                             %subst(vlrppt:posDaDft+1:
050700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800141028     C                   ENDIF
050900141028     C* GCA
051000141028     C                   IF        %subst(
051100141028     C                             %subst(vlrppt:posDaDft+1:
051200141028     C                             posADft-posDaDft-1):1:3)
051300141028     C                             = 'GCA'
051400141028     C                   EVAL      VABGCA=%trim(%subst(
051500141028     C                             %subst(vlrppt:posDaDft+1:
051600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700141028     C                   ENDIF
051800141028     C* CTM
051900141028     C                   IF        %subst(
052000141028     C                             %subst(vlrppt:posDaDft+1:
052100141028     C                             posADft-posDaDft-1):1:3)
052200141028     C                             = 'CTM'
052300141028     C                   EVAL      VABCTM=%trim(%subst(
052400141028     C                             %subst(vlrppt:posDaDft+1:
052500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600141028     C                   ENDIF
052700141028     C* FFD
052800141028     C                   IF        %subst(
052900141028     C                             %subst(vlrppt:posDaDft+1:
053000141028     C                             posADft-posDaDft-1):1:3)
053100141028     C                             = 'FFD'
053200141028     C                   EVAL      VABFFD=%trim(%subst(
053300141028     C                             %subst(vlrppt:posDaDft+1:
053400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500141028     C                   ENDIF
053600141028     C* VAD
053700141028     C                   IF        %subst(
053800141028     C                             %subst(vlrppt:posDaDft+1:
053900141028     C                             posADft-posDaDft-1):1:3)
054000141028     C                             = 'VAD'
054100141028     C                   EVAL      VABVAD=%trim(%subst(
054200141028     C                             %subst(vlrppt:posDaDft+1:
054300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400141028     C                   ENDIF
054500141028     C* GMA
054600141028     C                   IF        %subst(
054700141028     C                             %subst(vlrppt:posDaDft+1:
054800141028     C                             posADft-posDaDft-1):1:3)
054900141028     C                             = 'GMA'
055000141028     C                   EVAL      VABGMA=%trim(%subst(
055100141028     C                             %subst(vlrppt:posDaDft+1:
055200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300141028     C                   ENDIF
055400141028     C* GGA
055500141028     C                   IF        %subst(
055600141028     C                             %subst(vlrppt:posDaDft+1:
055700141028     C                             posADft-posDaDft-1):1:3)
055800141028     C                             = 'GGA'
055900141028     C                   EVAL      VABGGA=%trim(%subst(
056000141028     C                             %subst(vlrppt:posDaDft+1:
056100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200141028     C                   ENDIF
056300141028     C* GVA
056400141028     C                   IF        %subst(
056500141028     C                             %subst(vlrppt:posDaDft+1:
056600141028     C                             posADft-posDaDft-1):1:3)
056700141028     C                             = 'GVA'
056800141028     C                   EVAL      VABGVA=%trim(%subst(
056900141028     C                             %subst(vlrppt:posDaDft+1:
057000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100141028     C                   ENDIF
057200141028     C* TC1
057300141028     C                   IF        %subst(
057400141028     C                             %subst(vlrppt:posDaDft+1:
057500141028     C                             posADft-posDaDft-1):1:3)
057600141028     C                             = 'TC1'
057700141028     C                   EVAL      VABTC1=%trim(%subst(
057800141028     C                             %subst(vlrppt:posDaDft+1:
057900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000141028     C                   ENDIF
058100141028     C* TC2
058200141028     C                   IF        %subst(
058300141028     C                             %subst(vlrppt:posDaDft+1:
058400141028     C                             posADft-posDaDft-1):1:3)
058500141028     C                             = 'TC2'
058600141028     C                   EVAL      VABTC2=%trim(%subst(
058700141028     C                             %subst(vlrppt:posDaDft+1:
058800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900141028     C                   ENDIF
059000141028     C* VATTRC
059100141028     C                   IF        %subst(
059200141028     C                             %subst(vlrppt:posDaDft+1:
059300141028     C                             posADft-posDaDft-1):1:3)
059400141028     C                             = 'TRC'
059500141028     C                   EVAL      VATTRC=%trim(%subst(
059600141028     C                             %subst(vlrppt:posDaDft+1:
059700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800141028     C                   ENDIF
059900141028     C* ...
060000141028     C                   ENDIF
060100141028     C                   ENDDO
060200141028     C*
060300141028     C                   ENDSR
060400000801     C*----------------------------------------------------*
060500140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
060600000801     C*----------------------------------------------------*
060700040910     C     IMPVABVAT     BEGSR
060800040910     C*
060900040910     C* Traduzione relativa ai tipi record del file del cliente
061000040910     C*
061100071210     C*
061200071210     C***
061300141028     C* ...tipo record 'UNB' (info CMR)
061400141028     C                   EVAL      wTag = 'UNB'
061500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061600141028     C                   MOVEL     *blanks       savCMR           35
061700141028     C                   EVAL      InStringa =
061800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
061900141028     C                   EVAL      InSepara = '+'
062000141028     C                   EXSR      SR_SPLIT
062100141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
062200140922     C                                      %char(datcor) + ' ' + %char(oracor)
062300130620     C                   ENDIF
062400141028     C*
062500141028     C***
062600150526     C* ...tipo record 'CNI' (rottuta di codice spedizione)
062700150526     C                   EVAL      wTag = 'CNI'
062800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062900150526     C*
063000150526     C* Scarico buffer spedizione elaborata
063100150526     C                   IF        *IN90
063200150526     C  N31              ADD       1             §CTROKVB
063300150526     C  N31              exsr      CHKWRI
063400150526     C  N31              WRITE     EDIVAB00
063500150526     C                   ENDIF
063600150526     C                   SETON                                        90
063700150526     C*
063800150526     C* Inizializzazioni per nuova spedizione
063900141028     C                   SETOFF                                       31
064000141028     C                   eval      vinflg = '1'
064100141028     C* ......inizializzazioni iniziali e formati record file Bartolini
064200141028     C                   EXSR      INZVAR
064300141028     C* ......valorizzazione campi fissi
064400141028     C                   MOVEL     datcor        VABAAS
064500141028     C                   MOVE      datcor        VABMGS
064600141028     C                   MOVE(P)   vlrpoi        VABFGS
064700141028     C                   EVAL      VABCMR = savCMR
064800141028     C                   EVAL      VABDCM = datcor
064900141028     C                   EVAL      VABDTS = datcor
065000141028     C                   EVAL      VABCNT = 1
065100141028     C* ......NSP => Stacco un numeratore da AZNUM
065200141028     C                   clear                   TRUL33DS
065300141028     C                   eval      I33OPE = *zeros
065400141028     C                   eval      I33CNU = 302
065500141028     C                   eval      I33NUM = 1
065600141028     C                   movel     TRUL33DS      KPJBU
065700141028     C                   call      'TRUL33R'
065800141028     C                   parm                    KPJBA
065900141028     C                   movel     KPJBU         TRUL33DS
066000141028     C                   if        O33ERR = *zeros
066100141028     C                   z-add     O33NRF        VABNSP
066200141028     C                   else
066300141028     C                   Z-ADD     1             wRecErr
066400141028     C                   SETON                                        31
066500141028     C                   EVAL      vinmsg = %trimr(vinmsg)
066600141028     C                             + ' ' + 'VABNSP VATNSP'
066700141028     C                   endif
066800141028     C                   ENDIF
066900141028     C*
067000141028     C***
067100150526     C* ...tipo record 'RFF+FF' (riferimento spedizione)
067200170508     C***                EVAL      wTag = 'RFF+FF'
067300170508     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067400170508     C***                EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
067500170508     C***                EXSR      CHKNUM
067600170508     C***                IF        PiInt=*on
067700170508     C***                Z-ADD     PiVal         VABRMN
067800170508     C***                ELSE
067900170508     C***                EVAL      VABRMN = 1
068000170508     C***                Z-ADD     1             wRecErr
068100170508     C***                ENDIF
068200170508     C***                ENDIF
068300150526     C*
068400150526     C***
068500150526     C* ...tipo record 'RFF+CR' (riferimento spedizione)
068600161019     C***                EVAL      wTag = 'RFF+CR'
068700161019     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068800161019     C***                EVAL      VABRMA =
068900161019     C***                             %subst(wvindta:%len(%trim(wTag))+2)
069000161019     C***                ENDIF
069100161019     C*
069200161019     C***
069300161027     C* ...tipo record 'CNI' (riferimento spedizione)
069400161027     C                   EVAL      wTag = 'CNI'
069500161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069600161027     C                   EVAL      InStringa =
069700161027     C                                %subst(wvindta:%len(%trim(wTag))+2)
069800161027     C                   EVAL      InSepara = '+'
069900161027     C                   EXSR      SR_SPLIT
070000170508     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(2))
070100161019     C                   ENDIF
070200150527     C*
070300150527     C***
070400170510     C* ...tipo record 'FTX+DIN++810' (data consegna richiesta)
070500170510     C                   EVAL      wTag = 'FTX+DIN++810'
070600150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070700170510     C                   MOVEL     *blanks       wDATA             8
070800170510     C                   EVAL      wDATA=%subst(
070900170510     C                                 %subst(wvindta:%len(%trim(wTag))+2):1:8)
071000170510     C                   EVAL      PiStr=%subst(wDATA:5:4)+
071100170510     C                                   %subst(wDATA:3:2)+
071200170510     C                                   %subst(wDATA:1:2)
071300150527     C                   EXSR      CHKNUM
071400150527     C                   IF        PiInt=*on
071500150527     C                   Z-ADD     PiVal         VABDCR
071600150527     C                   ELSE
071700150527     C                   Z-ADD     1             wRecErr
071800150527     C                   ENDIF
071900150527     C                   ENDIF
072000170510     C*
072100170510     C***
072200170510     C* ...tipo record 'FTX+DIN++F15' (ora consegna richiesta)
072300170510     C                   EVAL      wTag = 'FTX+DIN++F15'
072400170510     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072500170510     C                   EVAL      PiStr=%subst(
072600170510     C                                 %subst(wvindta:%len(%trim(wTag))+2):1:4)
072700170510     C                   EXSR      CHKNUM
072800170510     C                   IF        PiInt=*on
072900170510     C                   Z-ADD     PiVal         VABHCR
073000170510     C                   ELSE
073100170510     C                   Z-ADD     1             wRecErr
073200170510     C                   ENDIF
073300170510     C                   ENDIF
073400170510     C*
073500170510     C***
073600170510     C* ...tipo record 'FTX+DIN++F51' (sponda idraulica)
073700170510     C                   EVAL      wTag = 'FTX+DIN++F51'
073800170510     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073900170510     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
074000170510     C                                     'CONSEGNA CON SPONDA IDRAULICA'
074100170510     C                   ENDIF
074200170510     C*
074300170510     C***
074400170510     C* ...tipo record 'FTX+DIN++833' (appuntamento)
074500170510     C                   EVAL      wTag = 'FTX+DIN++833'
074600170510     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074700170510     C                   EVAL      VABTC1 = 'A'
074800170510     C                   EVAL      InStringa =
074900170510     C                                %subst(wvindta:%len(%trim(wTag))+2)
075000170510     C                   EVAL      InSepara = '+'
075100170510     C                   EXSR      SR_SPLIT
075200170510     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
075300170510     C                   EVAL      VATTRC = 'B'
075400170510     C                   EXSR      WRIVAT
075500170510     C                   ENDIF
075600141028     C*
075700141028     C***
075800141028     C* ...tipo record 'NAD+CN' (destinatario)
075900141028     C                   EVAL      wTag = 'NAD+CN'
076000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076100141028     C                   EVAL      InStringa =
076200141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
076300141028     C                   EVAL      InSepara = '+'
076400141028     C                   EXSR      SR_SPLIT
076500170522     C                   MOVEL     *blanks       wStrngaSav01    256
076600170522     C                   MOVEL     *blanks       wStrngaSav02    256
076700170522     C                   EVAL      wStrngaSav01 = %trim(SkSplitCSV_5(3))
076800170522     C                   EVAL      wStrngaSav02 = %trim(SkSplitCSV_5(4))
076900141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
077000141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
077100141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
077200141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
077300141028     C                   IF        VABNZD = 'I'   OR
077400141028     C                             VABNZD = 'IT'  OR
077500141028     C                             VABNZD = 'ITA'
077600141028     C                   EVAL      VABNZD = *blanks
077700141028     C                   ENDIF
077800170522     C                   EVAL      InStringa = wStrngaSav01
077900141028     C                   EVAL      InSepara = ':'
078000141028     C                   EXSR      SR_SPLIT
078100141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
078200141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
078300141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
078400141028     C                                      %trim(SkSplitCSV_5(4))+
078500141028     C                                      %trim(SkSplitCSV_5(5))
078600170522     C                   EVAL      InStringa = wStrngaSav02
078700170522     C                   EVAL      InSepara = ':'
078800170522     C                   EXSR      SR_SPLIT
078900170522     C                   EVAL      VABIND = %trim(SkSplitCSV_5(1))
079000141028     C                   ENDIF
079100150609     C*
079200150609     C***
079300150609     C* ...tipo record 'NAD+CZ' (mittente originale)
079400150609     C                   EVAL      wTag = 'NAD+CZ'
079500150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079600150609     C                   EVAL      InStringa =
079700150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
079800150609     C                   EVAL      InSepara = '+'
079900150609     C                   EXSR      SR_SPLIT
080000150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
080100150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
080200150609     C                   IF        VABNMO = 'I'   OR
080300150609     C                             VABNMO = 'IT'  OR
080400150609     C                             VABNMO = 'ITA'
080500150609     C                   EVAL      VABNMO = *blanks
080600161018     C                   ELSE
080700161018     C                   Z-ADD     1             jNAZ
080800161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
080900161018     C                   IF        %found
081000161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
081100161018     C                   ENDIF
081200161018     C                   ENDIF
081300161018     C*
081400150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
081500150609     C                   EVAL      InSepara = ':'
081600150609     C                   EXSR      SR_SPLIT
081700150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
081800150609     C                   ENDIF
081900141104     C*
082000141104     C***
082100141104     C* ...tipo record 'NAD+UC' (destinatario)
082200141104     C                   EVAL      wTag = 'NAD+UC'
082300141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082400141104     C                   EVAL      InStringa =
082500141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
082600141104     C                   EVAL      InSepara = '+'
082700141104     C                   EXSR      SR_SPLIT
082800141104     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
082900141104     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
083000141104     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
083100141104     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
083200141104     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
083300141104     C                   IF        VABNZD = 'I'   OR
083400141104     C                             VABNZD = 'IT'  OR
083500141104     C                             VABNZD = 'ITA'
083600141104     C                   EVAL      VABNZD = *blanks
083700141104     C                   ENDIF
083800170522 xxx C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
083900141104     C                   EVAL      InSepara = ':'
084000141104     C                   EXSR      SR_SPLIT
084100141104     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
084200141104     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
084300141104     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
084400141104     C                                      %trim(SkSplitCSV_5(4))+
084500141104     C                                      %trim(SkSplitCSV_5(5))
084600141104     C                   ENDIF
084700141028     C*
084800141028     C***
084900141028     C* ...tipo record 'GID' (colli)
085000150609     C                   EVAL      wTag = 'GID+'
085100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085200141028     C                   EVAL      InStringa =
085300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
085400141028     C                   EVAL      InSepara = '+'
085500141028     C                   EXSR      SR_SPLIT
085600150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
085700150526     C                   EVAL      InSepara = ':'
085800150526     C                   EXSR      SR_SPLIT
085900150526     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
086000141028     C                   EXSR      CHKNUM
086100141028     C                   IF        PiInt=*on
086200150526     C                   ADD       PiVal         VABNCL
086300141028     C                   ELSE
086400141028     C                   Z-ADD     1             wRecErr
086500141028     C                   ENDIF
086600141028     C                   ENDIF
086700150527     C*
086800150527     C***
086900170403     C* ...tipo record 'MOA+22' (contrassegno / divisa)
087000170508     C***                EVAL      wTag = 'MOA+22'
087100170508     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087200170508     C***                EVAL      wFlgCAS = '1'
087300170508     C***                EVAL      InStringa =
087400170508     C***                             %subst(wvindta:%len(%trim(wTag))+2)
087500170508     C***                EVAL      InSepara = ':'
087600170508     C***                EXSR      SR_SPLIT
087700170508     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
087800170508     C***                EXSR      CHKNUM
087900170508     C***                IF        PiNum=*on
088000170508     C***                ADD       PiVal         VABCAS
088100170508     C***                EVAL      VABVCA = %trim(SkSplitCSV_5(2))
088200170508     C***                ELSE
088300170508     C***                Z-ADD     1             wRecErr
088400170508     C***                ENDIF
088500170508     C***                ENDIF
088600141028     C*
088700141028     C***
088800141028     C* ...tipo record 'FTX+AAA' (natura merce)
088900170510     C***                EVAL      wTag = 'FTX+AAA'
089000170510     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089100170510     C***                EVAL      InStringa =
089200170510     C***                             %subst(wvindta:%len(%trim(wTag))+2)
089300170510     C***                EVAL      InSepara = '+'
089400170510     C***                EXSR      SR_SPLIT
089500170510     C***                EVAL      VABNAS = %trim(SkSplitCSV_5(3))
089600170510     C***                ENDIF
089700161013     C*
089800161013     C***
089900161013     C* ...tipo record 'COM' (email destinatario ai fini alert)
090000170510     C***                EVAL      wTag = 'COM'
090100170510     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090200170510     C***                EVAL      InStringa =
090300170510     C***                             %subst(wvindta:%len(%trim(wTag))+2)
090400170510     C***                EVAL      InSepara = ':'
090500170510     C***                EXSR      SR_SPLIT
090600170510     C***                IF        SkSplitCSV_5(2) = 'EM'
090700170510     C***                EVAL      wEMAIL = SkSplitCSV_5(1)
090800170510     C***                EVAL      VATNOT = %subst(wEMAIL:1:35)
090900170510     C***                EVAL      VATTRC = 'I'
091000170510     C***                EXSR      WRIVAT
091100170510     C***                EVAL      VATNOT = %subst(wEMAIL:36:35)
091200170510     C***                EVAL      VATTRC = 'J'
091300170510     C***                EXSR      WRIVAT
091400170510     C***                ENDIF
091500170510     C***                ENDIF
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
092800141028     C*
092900141028     C***
093000161013     C* ...tipo record 'MEA+WT+AAD' (peso spedizione)
093100161013     C                   EVAL      wTag = 'MEA+WT+AAD'
093200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093300141028     C                   EVAL      InStringa =
093400141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
093500141028     C                   EVAL      InSepara = ':'
093600141028     C                   EXSR      SR_SPLIT
093700141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
093800141028     C                   EXSR      CHKNUM
093900141028     C                   IF        PiNum=*on
094000150924     C                   ADD       PiVal         VABPKB
094100141028     C                   ELSE
094200141028     C                   Z-ADD     1             wRecErr
094300141028     C                   ENDIF
094400141028     C                   ENDIF
094500141028     C*
094600141028     C***
094700141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
094800141028     C                   EVAL      wTag = 'MEA+VOL'
094900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095000141028     C                   EVAL      InStringa =
095100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
095200141028     C                   EVAL      InSepara = ':'
095300141028     C                   EXSR      SR_SPLIT
095400141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
095500141028     C                   EXSR      CHKNUM
095600141028     C                   IF        PiNum=*on
095700150924     C                   ADD       PiVal         VABVLB
095800141028     C                   ELSE
095900141028     C                   Z-ADD     1             wRecErr
096000141028     C                   ENDIF
096100141028     C                   ENDIF
096200141028     C*
096300141028     C***
096400161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
096500161013     C                   EVAL      wTag = 'GIN+BJ'
096600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
096700150526     C                   EVAL      InStringa =
096800150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
096900161014     C     '+':':'       XLATE     InStringa     InStringa
097000161013     C                   EVAL      InSepara = ':'
097100150526     C                   EXSR      SR_SPLIT
097200150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
097300150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
097400141028     C                   EVAL      VATTRC = 'E'
097500141028     C                   EXSR      WRIVAT
097600150526     C                   ENDIF
097700150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
097800150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
097900150526     C                   EVAL      VATTRC = 'E'
098000150526     C                   EXSR      WRIVAT
098100150526     C                   ENDIF
098200150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
098300150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
098400150526     C                   EVAL      VATTRC = 'E'
098500150526     C                   EXSR      WRIVAT
098600150526     C                   ENDIF
098700150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
098800150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
098900150526     C                   EVAL      VATTRC = 'E'
099000150526     C                   EXSR      WRIVAT
099100150526     C                   ENDIF
099200150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
099300150526     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
099400150526     C                   EVAL      VATTRC = 'E'
099500150526     C                   EXSR      WRIVAT
099600150526     C                   ENDIF
099700161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
099800161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(6))
099900161017     C                   EVAL      VATTRC = 'E'
100000161017     C                   EXSR      WRIVAT
100100161017     C                   ENDIF
100200161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
100300161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(7))
100400161017     C                   EVAL      VATTRC = 'E'
100500161017     C                   EXSR      WRIVAT
100600161017     C                   ENDIF
100700161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
100800161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(8))
100900161017     C                   EVAL      VATTRC = 'E'
101000161017     C                   EXSR      WRIVAT
101100161017     C                   ENDIF
101200161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
101300161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(9))
101400161017     C                   EVAL      VATTRC = 'E'
101500161017     C                   EXSR      WRIVAT
101600161017     C                   ENDIF
101700161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
101800161017     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(10))
101900161017     C                   EVAL      VATTRC = 'E'
102000161017     C                   EXSR      WRIVAT
102100161017     C                   ENDIF
102200141028     C                   ENDIF
102300141028     C*
102400141028     C***
102500141028     C* ...tipo record '???' (PDF per packing-list)
102600141028     C                   EVAL      wTag = '???PDF'
102700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102800141028     C*
102900141028     C* ......VATNOT_P
103000141028     C                   if        *in53
103100141028     C*
103200141028     C* Solo se valorizzato RMN
103300141028     C                   if        VABRMA <> *blanks
103400141028     C                   eval      pIn_MaskPDF =
103500141028     C                                     %trim(%subst(wvindta:1292:100))
103600141028     C                   call      'TIS7P2SER'
103700141028     C                   parm      'W'           pIn_Opz           1
103800141028     C                   parm                    tivlrds
103900141028     C                   parm                    EDIVABDS
104000141028     C                   parm      '10'          pIn_CdIdAz        2
104100141028     C                   parm                    pIn_MaskPDF      50
104200141028     C                   parm      *blanks       pOut_Esito        1
104300141028     C                   endif
104400141028     C                   endif
104500141028     C*
104600141028     C                   ENDIF
104700141028     C*
104800141028     C***
104900141028     C* ...tipo record 'UNT' (fine bolla)
105000141028     C                   EVAL      wTag = 'UNT'
105100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
105200150526     C                   IF        *IN90
105300141028     C  N31              ADD       1             §CTROKVB
105400141028     C  N31              exsr      CHKWRI
105500140117     C  N31              WRITE     EDIVAB00
105600150526     C                   ENDIF
105700150526     C                   ENDIF
105800010202     C*
105900000801     C* Ebbene...
106000000801     C                   ADD       1             §CTRMO
106100141028     C                   IF        wRecErr <> *zeros
106200000801     C                   ADD       1             §CTRNO
106300000801     C                   EVAL      vinflg = '2'
106400000801     C                   ENDIF
106500000801     C*
106600000801     C                   ENDSR
106700010202     C*----------------------------------------------------*
106800140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
106900010202     C*----------------------------------------------------*
107000020305     C     PREVAT        BEGSR
107100010202     C*
107200140117     C* Compongo il nome del membro da dare al EDIVATWR
107300010202     C                   eval      parmbr = vlrhdl
107400010202     C                   movel     'M'           parmbr
107500060113     C                   eval      parccm = vlrksc
107600010202     C                   eval      paropz = '1'
107700010202     C* Effettuo la chiamata al CLLE preposto
107800140117     C                   call(e)   'TITVEVTC'
107900010202     C                   parm                    parccm
108000010202     C                   parm                    parmbr
108100010202     C                   parm                    paropz
108200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
108300010202     C                   if        %error
108400010202     C                   movel     '1'           chkcall
108500010202     C                   else
108600010202     C                   movel     '0'           chkcall
108700010202     C                   endif
108800010202     C*
108900010202     C                   ENDSR
109000141028     C*----------------------------------------------------*
109100141028     C*  CONTROLLO NUMERICITA' CAMPI
109200141028     C*----------------------------------------------------*
109300141028     C     CHKNUM        BEGSR
109400141028     C*
109500141028     C                   IF        PiDecChr = *blanks
109600141028     C                   EVAL      PiDecChr = CharNUM
109700141028     C                   ENDIF
109800141028     C*
109900141028     C                   callp     UBISNUM_Check(PiStr
110000141028     C                                          :PiDecChr
110100141028     C                                          :PiVal
110200141028     C                                          :PiNum
110300141028     C                                          :PiInt)
110400141028     C*
110500141028     C                   ENDSR
110600141028     C***
110700141028
110800141028
110900141028     C*----------------------------------------------------*
111000141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
111100141028     C*----------------------------------------------------*
111200141028     C     SR_SPLIT      BEGSR
111300141028     C*
111400141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
111500141028     C                   CALL      'XSPLIT2'
111600141028     C                   PARM                    InStringa
111700141028     C                   PARM                    InSepara
111800141028     C                   PARM      '5'           InTypeOut
111900141028     C                   PARM      ''            wSkParam
112000141028     C                   PARM                    OutErrore
112100141028     C                   PARM                    OutMsg
112200141028     C                   MOVEA     wSkParam      SkSplitCSV_5
112300141028     C*
112400141028     C                   ENDSR
112500141028
112600141028
112700141028     C*----------------------------------------------------*
112800141028     C*  SCRITTURA BUFFER EDIVAT
112900141028     C*----------------------------------------------------*
113000141028     C     WRIVAT        BEGSR
113100141028     C*
113200141028     C                   IF        VATNOT <> *blanks
113300141028     C                   EVAL      VATFGS = VABFGS
113400141028     C                   EVAL      VATCCM = VABCCM
113500141028     C                   EVAL      VATLNP = VABLNP
113600141028     C                   EVAL      VATAAS = VABAAS
113700141028     C                   EVAL      VATNRS = VABNRS
113800141028     C                   EVAL      VATNSP = VABNSP
113900141028     C                   EVAL      VATCMR = VABCMR
114000141028     C                   EVAL      VATCNT = VABCNT
114100141028     C                   ADD       1             §CTROKVT
114200141028     C                   WRITE     EDIVAT00
114300141028     C                   ENDIF
114400141028     C*
114500141028     C                   ENDSR
114600000801
114700011113
114800011113     C*----------------------------------------------------*
114900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
115000011113     C*----------------------------------------------------*
115100011113     C     CHKIMPDIV     BEGSR
115200011113     C*
115300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
115400011113     C                   Z-ADD     *zeros        wrkDec            9 9
115500011113     C*
115600011113     C* Come prima cosa effettuo considerazioni sulla divisa
115700011113     C                   IF        vabIAS > *zeros
115800011113     C                   IF        vabVAS <> 'EUR'
115900011113     C                   EVAL      vabVAS =  'ITL'
116000011113     C                   ENDIF
116100011113     C                   ENDIF
116200011113     C*
116300011113     C                   IF        vabCAS > *zeros
116400011113     C                   IF        vabVCA <> 'EUR'
116500011113     C                   EVAL      vabVCA =  'ITL'
116600011113     C                   ENDIF
116700011113     C                   ENDIF
116800011113     C*
116900011113     C                   IF        vabVMD > *zeros
117000020305     C                   IF        vabVAD <> 'EUR'
117100011113     C                   EVAL      vabVAD =  'ITL'
117200011113     C                   ENDIF
117300011113     C                   ENDIF
117400011113     C*
117500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
117600011113     C                   Z-ADD     vabIAS        wrkDec
117700011113     C                   IF        wrkDec > *zeros
117800011113     C                   IF        vabVAS = 'ITL'
117900011113     C                   EVAL      vabIAS = *zeros
118000011113     C                   ENDIF
118100011113     C                   ENDIF
118200011113     C*
118300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
118400011113     C                   Z-ADD     vabCAS        wrkDec
118500011113     C                   IF        wrkDec > *zeros
118600011113     C                   IF        vabVCA = 'ITL'
118700011113     C                   EVAL      vabCAS = *zeros
118800011113     C                   ENDIF
118900011113     C                   ENDIF
119000011113     C*
119100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
119200011113     C                   Z-ADD     vabVMD        wrkDec
119300011113     C                   IF        wrkDec > *zeros
119400011113     C                   IF        vabVAD = 'ITL'
119500011113     C                   EVAL      vabVMD = *zeros
119600011113     C                   ENDIF
119700011113     C                   ENDIF
119800011113     C*
119900011113     C                   ENDSR
120000011113     C***
120100011113
120200011113
120300000801
120400000801
120500990920      /TITLE Invio dei dati al punto operativo.
120600010202     C     invio         BEGSR
120700990920     C*
120800140117     C* 1° invio EDIVAT
120900010201     C                   reset                   dscmz
121000010201     C                   move      vlrpoi        cmzdst
121100140117     C                   eval      cmzfld = 'EDIVATWR'
121200010201     C                   eval      cmzmbd = vlrhdl
121300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
121400021009     C***                if        prmfir = *blanks
121500140117     C                   eval      cmzfla = 'EDIVAT0F'
121600140117     C                   eval      cmzmba = 'EDIVAT0F'
121700021009     C***                else
121800021009     C***                eval      cmzfla = prmfir
121900021009     C***                eval      cmzmba = prmfir
122000021009     C***                endif
122100010201     C                   eval      cmznrr = *zeros
122200020305     C                   move      §ctrokvt      cmznrr
122300021018     C                   eval      cmzlba = vlrfl1
122400010201     C                   call(e)   'TIS711C'
122500010201     C                   parm                    dscmz
122600010201     C                   parm      *blanks       esito
122700010205     C                   if        %error
122800010205     C                             or cmzerr = '1'
122900010205     C                             or esito  = '1'
123000010205     C                   eval      wrkesito = '3'
123100010205     C                   else
123200010201     C*
123300140117     C* 2° invio EDIVAB
123400010201     C                   reset                   dscmz
123500010201     C                   move      vlrpoi        cmzdst
123600010201     C                   eval      cmzfld = vlrfou
123700010201     C                   eval      cmzmbd = vlrhdl
123800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
123900021009     C***                if        prmfir = *blanks
124000140117     C                   eval      cmzfla = 'EDIVAB0F'
124100140117     C                   eval      cmzmba = 'EDIVAB0F'
124200021009     C***                else
124300021009     C***                eval      cmzfla = prmfir
124400021009     C***                eval      cmzmba = prmfir
124500021009     C***                endif
124600010201     C                   eval      cmznrr = *zeros
124700010201     C                   move      §ctrokvb      cmznrr
124800021018     C                   eval      cmzlba = vlrfl1
124900010201     C                   call(e)   'TIS711C'
125000010201     C                   parm                    dscmz
125100010201     C                   parm      *blanks       esito
125200010201     C                   if        %error
125300010201     C                             or cmzerr = '1'
125400010201     C                             or esito  = '1'
125500010201     C                   eval      wrkesito = '3'
125600010201     C                   endif
125700010205     C                   endif
125800990920     C*
125900000613     C                   ENDSR
126000000613     C***
126100070411
126200141028
126300070411     C     *pssr         BEGSR
126400070411     C*
126500070411     C                   if        %open(tivin00r)
126600070411     C                   close     tivin00r
126700070411     C                   endif
126800140117     C                   if        %open(edivabwr)
126900140117     C                   close     edivabwr
127000070411     C                   endif
127100140117     C                   if        %open(edivatwr)
127200140117     C                   close     edivatwr
127300070411     C                   endif
127400070411     C*
127500070411     C* Effettuo la chiamata al CLLE preposto
127600140117     C                   call(e)   'TITVEVTC'
127700070411     C                   parm                    parccm
127800070411     C                   parm                    parmbr
127900070411     C                   parm      '2'           paropz
128000070411     C*
128100070411     C                   eval      wrkesito = '2'
128200070411     C*
128300070411     C                   seton                                        LR
128400070411     C*
128500070411     C                   ENDSR     '*CANCL'
128600070411     C***
128700161018
128800161018
128900161018     C*--------------------------------------------------------
129000161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
129100161018     C*--------------------------------------------------------
129200161018     C     CARTAB        BEGSR
129300161018     C*
129400161018     C* TABELLA '15' - NAZIONI
129500161018     C                   clear                   skNAZISO
129600161018     C                   clear                   skNAZBAR
129700161018     C                   eval      tblKUT = 1
129800161018     C                   eval      tblCOD = '15'
129900161018     C     KEYtabP       setll     tabel00f
130000161018     C     KEYtabP       reade     tabel00f
130100161018     C                   dow       not %eof(tabel00f)
130200161018     C                   if        tblFLG = *blanks
130300161018     C                   movel(p)  tblUNI        ds15
130400161018     C                   add       1             jNAZ
130500161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
130600161018     C                   eval      skNAZISO(jNAZ) = §15COD
130700161018     C                   endif
130800161018     C     KEYtabP       reade     tabel00f
130900161018     C                   enddo
131000161018     C*
131100161018     C                   ENDSR
131200161018     C***
131300070411
131400990910
131500000613     C     *inzsr        BEGSR
131600990910     C*
131700990910     C     *entry        plist
131800990920     C                   parm                    tivlrds
131900990921     C                   parm      wrkesito      esito
132000000724     C                   parm                    prmlit
132100000710     C                   parm                    prmfir
132200000613     C*
132300000830     C* CALCOLA LA DATA CORRENTE
132400140117     C                   time                    wn14             14 0
132500140117     C                   movel     wn14          oracor            6 0          *ORA
132600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
132700100722     C                   eval      datcor = %dec(%date() : *ISO)
132800161018     C*
132900161018     C* Chiave su TABEL00F - parziale
133000161018     C     KEYtabP       KLIST
133100161018     C                   KFLD                    tblKUT
133200161018     C                   KFLD                    tblCOD
133300000830     C*
133400000613     C                   ENDSR
133500000613     C***
