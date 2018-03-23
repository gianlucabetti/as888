000100180115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR/EDIVAD1WR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400161018     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140117     FEDIVABwr  O    E             DISK    usropn
000700171218     FEDIVATwr  O    E             DISK    usropn
000800180116     FEDIVAD1wr O    E             DISK    usropn prefix(H_)
000900180115     FEDIVADwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400180116     D***EDIVAD1wr      e ds                         prefix(H_)
001500180116
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070719     D tisi95ds      e ds
002100140117     D edivabds      e ds                  extname(edivab0f)
002200161018     D ds15          e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200141028     D wTag            s            128    VARYING INZ
003300141028     D wRecErr         s              1  0 INZ(*zeros)
003400141028     D wFlgCAS         s              1    INZ(*blanks)
003500141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003600150527     D wNOTE           s             70    INZ(*blanks)
003700161013     D wEMAIL          s             70    INZ(*blanks)
003800141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003900141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
004000141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
004100141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
004200141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
004300141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
004400141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004500171108     D wI              s              3s 0
004600171220     D wJ              s              3s 0
004700171218     D wRFF            s              3  0 INZ(*zeros)
004800161018
004900161018     D*------------
005000161018     D jNAZ            s              5  0 INZ(*zeros)
005100161018     D skNAZISO        S              3    DIM(1000)
005200161018     D skNAZBAR        S              3    DIM(1000)
005300141028
005400141028     D*------------------
005500141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600141028     D*------------------
005700141028     D CharNUM         S              1
005800141028     D CharSOS         S              1
005900141028     D posDaDft        S              3  0 INZ(*zeros)
006000141028     D posADft         S              3  0 INZ(*zeros)
006100141028     D wGiroDft        s              1  0 INZ(*zeros)
006200141028
006300141028
006400141028     D*------------------
006500141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006600141028     D*------------------
006700141028     D InStringa       S          65535A   VARYING                              (stringa input)
006800141028     D InSepara        S             10A                                        (separatore)
006900141028     D InTypeOut       S              1A                                        (tipo output)
007000141028     D wSkParam        S          65535A                                        (output)
007100141028     D OutErrore       S              1A                                        (flag errore)
007200141028     D OutMsg          S             80A                                        (messaggio errore)
007300141028     D SkSplitCSV_5    S            256    DIM(256)
007400171220     D SkSplitCSV_Sav  S            256    DIM(256)
007500180116     D SkRFF           S             35    DIM(100)
007600141028
007700000830
007800041025     D*------------------
007900041025     D* DS REPERIMENTO NUMERATORE
008000041025     D*------------------
008100041025     D trul33ds      e ds                  inz
008200041025     D*------------------
008300041025     D* DS ARCHITETTURA
008400041025     D*------------------
008500041025     D kpjba         e ds                  inz
008600041025     D*------------------
008700990908
008800141028
008900141028     D*------------------
009000141028     D* LINKING A DEFINIZIONI ESTERNE
009100141028     D*------------------
009200141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009300141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009400150609
009500141028
009600150609     D*-------------------
009700150609     D* COSTANTI
009800150609     D*-------------------
009900150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010000150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010100141028
010200010201
010300010201
010400000913     C                   reset                   rrnum
010500990921     C                   reset                   esito
010600990921     C                   reset                   wrkesito
010700000613     C*
010800161018     C                   EXSR      CARTAB                                       CARICA TABELLE
010900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011000070719     C*
011100070719     C* Esegue lancio TISI95R solo x chiusura
011200070719     C                   CLEAR                   TISI95DS
011300070719     C                   EVAL      I95TLA = 'C'
011400070719     C                   CALL      'TISI95R'
011500070719     C                   PARM                    TISI95DS
011600000613     C*
011700010202     C* Effettuo la chiamata al CLLE preposto
011800180116     C                   call(e)   'TITVEVD1TC'
011900180116     C***lk              call(e)   'TITVEVDTC'
012000140922     C                   parm                    parccm
012100140922     C                   parm                    parmbr
012200140922     C                   parm      '2'           paropz
012300170731     C*
012400170731     C                   if        *in53
012500170731     C                   call      'TIS7P2SER'
012600170731     C                   parm      'C'           pIn_Opz           1
012700170731     C                   parm                    tivlrds
012800170731     C                   parm                    EDIVABDS
012900170731     C                   parm      *blanks       pIn_CdIdAz        2
013000170731     C                   parm      *blanks       pIn_MaskPDF      50
013100170731     C                   parm      *blanks       pOut_Esito        1
013200170731     C                   endif
013300000616     C*
013400010201     C                   seton                                        LR
013500990908
013600000801
013700910830     C*--------------------------------------------------------
013800171219     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR EDIVATWR e EDIVAD1WR   *
013900910830     C*--------------------------------------------------------
014000040526     C     RWFILE        BEGSR
014100990910     C*
014200990914     C                   if        not %open(tivin00r)
014300990908     C                   open      tivin00r
014400990914     C                   endif
014500140117     C                   if        not %open(edivabwr)
014600140117     C                   open      edivabwr
014700171219     C                   endif
014800171219     C*
014900180115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR/EDIVADWR/EDIVAD1WR
015000171219     C                   exsr      prevaD1T
015100171219     C*
015200171219     C                   if        chkcall = '0'
015300171219     C*
015400180116     C                   if        not %open(EDIVAD1wr)
015500180116     C                   open      EDIVAD1wr
015600180116     C                   endif
015700180116     C                   if        not %open(EDIVADwr)
015800180115     C                   open      EDIVADwr
015900180115     C                   endif
016000171219     C                   if        not %open(EDIVATwr)
016100171219     C                   open      EDIVATwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   clear                   §CTROKVB          5 0
016500020305     C                   clear                   §CTROKVT          5 0
016600180116     C                   clear                   §CTROKVD          5 0
016700180116     C                   clear                   §CTROKVD1         5 0
016800000801     C                   clear                   §CTRMO            5 0
016900000801     C                   clear                   §CTRNO            5 0
017000141028     C*
017100141028     C* Inizializzazioni fuori ciclo
017200141028     C                   EXSR      INZVAR
017300140117     C*
017400140902     C                   if        *in53
017500140117     C                   call      'TIS7P2SER'
017600140117     C                   parm      'O'           pIn_Opz           1
017700140117     C                   parm                    tivlrds
017800140117     C                   parm                    EDIVABDS
017900140117     C                   parm      *blanks       pIn_CdIdAz        2
018000140117     C                   parm      *blanks       pIn_MaskPDF      50
018100140117     C                   parm      *blanks       pOut_Esito        1
018200140902     C                   endif
018300130708     C*
018400921023     C                   DO        *HIVAL
018500990913     C*
018600990915     C                   READ      tivin00r                               70
018700040910     C                   if        vindta > *blanks
018800000613     C                   add       1             rrnum
018900000801     C*
019000000801     C                   if        *in70 = *off
019100000801     C                             and
019200000801     C                             (vinflg = *blanks
019300000801     C                              or vinflg = '0'
019400000801     C                              or vinflg = '2')
019500000801     C*
019600000801     C                   clear                   vinmsg
019700141028     C*
019800141028     C* "normalizzo" il dato di input
019900141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
020000141028     C                                             %len(%trim(vindta))-1))
020100160209     C*
020200160209     C* Elimino le "duple di controllo EDIFACT"
020300160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
020400160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
020500160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
020600160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
020700160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
020800040910     C*
020900040910     C* Eseguo routine d traduzione
021000171218     C                   exsr      impvabvad
021100040802     C*
021200010305     C                   endif
021300000905     C*
021400000905     C                   else
021500000905     C                   eval      vinflg = '1'
021600000905     C                   endif
021700000905     C*
021800000905     C  N70              update    tivin000
021900000905     C*
022000991022     C  N70              ENDdo
022100100722     C*
022200100722     C* Scarico i buffer testata ancora "in canna"
022300140922     C*
022400141028     C***  N31              ADD       1             §CTROKVB
022500141028     C***  N31              exsr      CHKWRI
022600141028     C***  N31              WRITE     EDIVAB00
022700010202     C*
022800010202     C                   endif
022900990910
023000990910     C* Se non ci sono record con errori ...
023100000710     C                   if        §ctrno = 0
023200990910     C* ... restituisco esito OK.
023300990921     C                   eval      wrkesito = '0'
023400990910     C                   else
023500010201     C                   if        §ctrokvb > 0
023600990921     C                   eval      wrkesito = '1'
023700000710     C                   else
023800000710     C                   eval      wrkesito = '2'
023900990910     C                   endif
024000000710     C                   endif
024100990910     C*
024200990914     C                   if        %open(tivin00r)
024300990908     C                   close     tivin00r
024400990914     C                   endif
024500140117     C                   if        %open(edivabwr)
024600140117     C                   close     edivabwr
024700990914     C                   endif
024800180116     C                   if        %open(EDIVAD1wr)
024900180116     C                   close     EDIVAD1wr
025000180116     C                   endif
025100180115     C                   if        %open(EDIVADwr)
025200180115     C                   close     EDIVADwr
025300180115     C                   endif
025400171219     C                   if        %open(EDIVATwr)
025500171219     C                   close     EDIVATwr
025600171219     C                   endif
025700990910     C*
025800010201     C                   if        §ctrokvb > 0
025900000724     C                             and vlrpoi <> *zeros
026000180116     C                   exsr      invio
026100990920     C                   endif
026200990920     C*
026300910830     C                   ENDSR
026400000613     C***
026500140922
026600140922     C*----------------------------------------------------*
026700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
026800140922     C*----------------------------------------------------*
026900140922     C     CHKWRI        BEGSR
027000140922     C*
027100141028     C                   if        VABRMN = *zeros
027200141028     C                   eval      VABRMN = VABNSP
027300141028     C                   endif
027400170214     C*
027500171031     C* Se peso non indicato nei dati cliente e richiesto un peso forzato per collo
027600170214     C                   IF        VABPKB = *zeros
027700170214     C                   EVAL      VABPKB = wVABPKN * VABNCL
027800170214     C                   ENDIF
027900141028     C*
028000141028     C                   if        VABPRD = *blanks
028100141028     C* ......VABPRD
028200141028     C* Reperisco la provincia dal CAP e dalla località
028300141028     C                   IF        VABLOD <> *blanks AND
028400141028     C                             VABCAD <> *blanks AND
028500141028     C                             VABNZD  = *blanks
028600141028     C                   CLEAR                   TISI95DS
028700141028     C                   EVAL      I95TCN = '3'
028800141028     C                   Z-ADD     datcor        I95DAT
028900141028     C                   EVAL      I95CAP = VABCAD
029000141028     C                   EVAL      I95LOC = VABLOD
029100141028     C                   CALL      'TISI95R'
029200141028     C                   PARM                    TISI95DS
029300141028     C                   EVAL      VABPRD = O95PRV
029400141028     C                   ENDIF
029500141028     C                   endif
029600140922     C*
029700140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029800141028     C                   IF        wFlgCAS <> '0'
029900140922     C                   IF        VABCBO = '1'
030000140922     C                   EVAL      VABCBO = '4'
030100140922     C                   ELSE
030200140922     C                   EVAL      VABCBO = '6'
030300140922     C                   ENDIF
030400140922     C                   ENDIF
030500140922     C*
030600140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030700141028     C                   EXSR      CHKIMPDIV
030800150527     C*
030900150527     C* Scompongo le note
031000150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
031100150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
031200140922     C*
031300140922     C                   ENDSR
031400141028     C*----------------------------------------------------*
031500141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031600141028     C*----------------------------------------------------*
031700141028     C     INZVAR        BEGSR
031800141028     C*
031900141028     C* Inizializzo variabili di wrk
032000141028     C                   Z-ADD     *zeros        Num5_0            5 0
032100141028     C                   MOVEL     '0'           wFlgCAS
032200141028     C                   MOVEL     *blanks       wVATNOT_A
032300141028     C                   MOVEL     *blanks       wVATNOT_B
032400141028     C                   MOVEL     *blanks       wVATNOT_E
032500141028     C                   MOVEL     *blanks       wVATNOT_I
032600141028     C                   MOVEL     *blanks       wVATNOT_J
032700141028     C                   MOVEL     *blanks       wVATNOT_S
032800141028     C                   MOVEL     *blanks       wVATNOT_P
032900141028     C                   CLEAR                   SkSplitCSV_5
033000150527     C                   MOVEL     *blanks       wNOTE
033100180116     C                   CLEAR                   SkRFF
033200180116     C                   CLEAR                   wRFF
033300141028     C*
033400141028     C* Reimposto i valori di default
033500141028     C                   EXSR      DEFCAM
033600141028     C*
033700141028     C                   ENDSR
033800141028     C*----------------------------------------------------*
033900141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
034000141028     C*----------------------------------------------------*
034100141028     C     DEFCAM        BEGSR
034200141028     C*
034300141028     C                   CLEAR                   EDIVAB00
034400171219     C                   CLEAR                   EDIVAT00
034500180115     C                   CLEAR                   EDIVAD00
034600180116     C*                  CLEAR                   EDIVAD100
034700141028     C*
034800141028     C* Imposto i valori bolla di default
034900141028     C                   EVAL      VABCTR = 000
035000141028     C                   EVAL      VABCBO = '1'
035100141104     C                   EVAL      VABTSP = 'C'
035200141028     C*
035300141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035400141028     C* e delimitatore testo.
035500141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035600141028     C*
035700141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
035800141028     C                   EVAL      CharSOS = CharNUM
035900141028     C*
036000141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
036100141028     C                   SETOFF                                       53
036200141028     C                   SELECT
036300141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
036400141028     C                   SETON                                        53
036500141028     C                   ENDSL
036600141028     C*
036700141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
036800141028     C                   EVAL      posDaDft = 1
036900141028     C                   EVAL      posADft  = 0
037000141028     C                   EVAL      wGiroDft = 0
037100141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
037200141028     C                             posDaDft > 0
037300141028     C*
037400141028     C* Gestisco il 1° giro
037500141028     C                   IF        wGiroDft = 0
037600141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
037700141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
037800141028     C* Incremento il contatore dei "giri"
037900141028     C                   EVAL      wGiroDft = 1
038000141028     C                   ELSE
038100141028     C                   EVAL      posDaDft = posADft
038200141028     C                   ENDIF
038300141028     C* Eseguo lo scan x trovare la fine del campo corrente
038400141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
038500141028     C*
038600141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
038700141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
038800141028     C                   IF        posDaDft > 0 AND
038900141028     C                             posADft  > 0
039000141028     C* NCL
039100141028     C                   IF        %subst(
039200141028     C                             %subst(vlrppt:posDaDft+1:
039300141028     C                             posADft-posDaDft-1):1:3)
039400141028     C                             = 'NCL'
039500141028     C                   EVAL      PiStr=%trim(%subst(
039600141028     C                             %subst(vlrppt:posDaDft+1:
039700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039800141028     C                   EXSR      CHKNUM
039900141028     C                   IF        PiInt=*on
040000141028     C                   Z-ADD     PiVal         VABNCL
040100141028     C                   ENDIF
040200141028     C                   ENDIF
040300141028     C* CCM
040400141028     C                   IF        %subst(
040500141028     C                             %subst(vlrppt:posDaDft+1:
040600141028     C                             posADft-posDaDft-1):1:3)
040700141028     C                             = 'CCM'
040800141028     C                   EVAL      PiStr=%trim(%subst(
040900141028     C                             %subst(vlrppt:posDaDft+1:
041000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041100141028     C                   EXSR      CHKNUM
041200141028     C                   IF        PiInt=*on
041300171219     C                   Z-ADD     PiVal         VABCCM
041400141028     C                   ENDIF
041500141028     C                   ENDIF
041600141028     C* LNP
041700141028     C                   IF        %subst(
041800141028     C                             %subst(vlrppt:posDaDft+1:
041900141028     C                             posADft-posDaDft-1):1:3)
042000141028     C                             = 'LNP'
042100141028     C                   EVAL      PiStr=%trim(%subst(
042200141028     C                             %subst(vlrppt:posDaDft+1:
042300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400141028     C                   EXSR      CHKNUM
042500141028     C                   IF        PiInt=*on
042600141028     C                   Z-ADD     PiVal         VABLNP
042700141028     C                   ENDIF
042800141028     C                   ENDIF
042900141028     C* NRS
043000141028     C                   IF        %subst(
043100141028     C                             %subst(vlrppt:posDaDft+1:
043200141028     C                             posADft-posDaDft-1):1:3)
043300141028     C                             = 'NRS'
043400141028     C                   EVAL      PiStr=%trim(%subst(
043500141028     C                             %subst(vlrppt:posDaDft+1:
043600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700141028     C                   EXSR      CHKNUM
043800141028     C                   IF        PiInt=*on
043900141028     C                   Z-ADD     PiVal         VABNRS
044000141028     C                   ENDIF
044100141028     C                   ENDIF
044200141028     C* CTR
044300141028     C                   IF        %subst(
044400141028     C                             %subst(vlrppt:posDaDft+1:
044500141028     C                             posADft-posDaDft-1):1:3)
044600141028     C                             = 'CTR'
044700141028     C                   EVAL      PiStr=%trim(%subst(
044800141028     C                             %subst(vlrppt:posDaDft+1:
044900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045000141028     C                   EXSR      CHKNUM
045100141028     C                   IF        PiInt=*on
045200141028     C                   Z-ADD     PiVal         VABCTR
045300141028     C                   ENDIF
045400141028     C                   ENDIF
045500141028     C* PKB
045600141028     C                   IF        %subst(
045700141028     C                             %subst(vlrppt:posDaDft+1:
045800141028     C                             posADft-posDaDft-1):1:3)
045900141028     C                             = 'PKB'
046000141028     C                   EVAL      PiStr=%trim(%subst(
046100141028     C                             %subst(vlrppt:posDaDft+1:
046200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300141028     C                   EXSR      CHKNUM
046400141028     C                   IF        PiNum=*on
046500141028     C                   Z-ADD     PiVal         VABPKB
046600141028     C                   ENDIF
046700141028     C                   ENDIF
046800141028     C* VLB
046900141028     C                   IF        %subst(
047000141028     C                             %subst(vlrppt:posDaDft+1:
047100141028     C                             posADft-posDaDft-1):1:3)
047200141028     C                             = 'VLB'
047300141028     C                   EVAL      PiStr=%trim(%subst(
047400141028     C                             %subst(vlrppt:posDaDft+1:
047500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600141028     C                   EXSR      CHKNUM
047700141028     C                   IF        PiNum=*on
047800141028     C                   Z-ADD     PiVal         VABVLB
047900141028     C                   ENDIF
048000141028     C                   ENDIF
048100141028     C* QFT
048200141028     C                   IF        %subst(
048300141028     C                             %subst(vlrppt:posDaDft+1:
048400141028     C                             posADft-posDaDft-1):1:3)
048500141028     C                             = 'QFT'
048600141028     C                   EVAL      PiStr=%trim(%subst(
048700141028     C                             %subst(vlrppt:posDaDft+1:
048800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900141028     C                   EXSR      CHKNUM
049000141028     C                   IF        PiNum=*on
049100141028     C                   Z-ADD     PiVal         VABQFT
049200141028     C                   ENDIF
049300141028     C                   ENDIF
049400141028     C* CBO
049500141028     C                   IF        %subst(
049600141028     C                             %subst(vlrppt:posDaDft+1:
049700141028     C                             posADft-posDaDft-1):1:3)
049800141028     C                             = 'CBO'
049900141028     C                   EVAL      VABCBO=%trim(%subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200141028     C                   ENDIF
050300141028     C* TSP
050400141028     C                   IF        %subst(
050500141028     C                             %subst(vlrppt:posDaDft+1:
050600141028     C                             posADft-posDaDft-1):1:3)
050700141028     C                             = 'TSP'
050800141028     C                   EVAL      VABTSP=%trim(%subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100141028     C                   ENDIF
051200141028     C* VAS
051300141028     C                   IF        %subst(
051400141028     C                             %subst(vlrppt:posDaDft+1:
051500141028     C                             posADft-posDaDft-1):1:3)
051600141028     C                             = 'VAS'
051700141028     C                   EVAL      VABVAS=%trim(%subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000141028     C                   ENDIF
052100141028     C* VCA
052200141028     C                   IF        %subst(
052300141028     C                             %subst(vlrppt:posDaDft+1:
052400141028     C                             posADft-posDaDft-1):1:3)
052500141028     C                             = 'VCA'
052600141028     C                   EVAL      VABVCA=%trim(%subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900141028     C                   ENDIF
053000141028     C* TIC
053100141028     C                   IF        %subst(
053200141028     C                             %subst(vlrppt:posDaDft+1:
053300141028     C                             posADft-posDaDft-1):1:3)
053400141028     C                             = 'TIC'
053500141028     C                   EVAL      VABTIC=%trim(%subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800141028     C                   ENDIF
053900141028     C* GCA
054000141028     C                   IF        %subst(
054100141028     C                             %subst(vlrppt:posDaDft+1:
054200141028     C                             posADft-posDaDft-1):1:3)
054300141028     C                             = 'GCA'
054400141028     C                   EVAL      VABGCA=%trim(%subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700141028     C                   ENDIF
054800141028     C* CTM
054900141028     C                   IF        %subst(
055000141028     C                             %subst(vlrppt:posDaDft+1:
055100141028     C                             posADft-posDaDft-1):1:3)
055200141028     C                             = 'CTM'
055300141028     C                   EVAL      VABCTM=%trim(%subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500171218     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600141028     C                   ENDIF
055700141028     C* FFD
055800141028     C                   IF        %subst(
055900141028     C                             %subst(vlrppt:posDaDft+1:
056000141028     C                             posADft-posDaDft-1):1:3)
056100141028     C                             = 'FFD'
056200141028     C                   EVAL      VABFFD=%trim(%subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500141028     C                   ENDIF
056600141028     C* VAD
056700141028     C                   IF        %subst(
056800141028     C                             %subst(vlrppt:posDaDft+1:
056900141028     C                             posADft-posDaDft-1):1:3)
057000141028     C                             = 'VAD'
057100141028     C                   EVAL      VABVAD=%trim(%subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400141028     C                   ENDIF
057500141028     C* GMA
057600141028     C                   IF        %subst(
057700141028     C                             %subst(vlrppt:posDaDft+1:
057800141028     C                             posADft-posDaDft-1):1:3)
057900141028     C                             = 'GMA'
058000141028     C                   EVAL      VABGMA=%trim(%subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300141028     C                   ENDIF
058400141028     C* GGA
058500141028     C                   IF        %subst(
058600141028     C                             %subst(vlrppt:posDaDft+1:
058700141028     C                             posADft-posDaDft-1):1:3)
058800141028     C                             = 'GGA'
058900141028     C                   EVAL      VABGGA=%trim(%subst(
059000141028     C                             %subst(vlrppt:posDaDft+1:
059100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200141028     C                   ENDIF
059300141028     C* GVA
059400141028     C                   IF        %subst(
059500141028     C                             %subst(vlrppt:posDaDft+1:
059600141028     C                             posADft-posDaDft-1):1:3)
059700141028     C                             = 'GVA'
059800141028     C                   EVAL      VABGVA=%trim(%subst(
059900141028     C                             %subst(vlrppt:posDaDft+1:
060000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100141028     C                   ENDIF
060200141028     C* TC1
060300141028     C                   IF        %subst(
060400141028     C                             %subst(vlrppt:posDaDft+1:
060500141028     C                             posADft-posDaDft-1):1:3)
060600141028     C                             = 'TC1'
060700141028     C                   EVAL      VABTC1=%trim(%subst(
060800141028     C                             %subst(vlrppt:posDaDft+1:
060900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000141028     C                   ENDIF
061100141028     C* TC2
061200141028     C                   IF        %subst(
061300141028     C                             %subst(vlrppt:posDaDft+1:
061400141028     C                             posADft-posDaDft-1):1:3)
061500141028     C                             = 'TC2'
061600141028     C                   EVAL      VABTC2=%trim(%subst(
061700141028     C                             %subst(vlrppt:posDaDft+1:
061800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900141028     C                   ENDIF
062000141028     C* VATTRC
062100141028     C                   IF        %subst(
062200141028     C                             %subst(vlrppt:posDaDft+1:
062300141028     C                             posADft-posDaDft-1):1:3)
062400141028     C                             = 'TRC'
062500141028     C                   EVAL      VATTRC=%trim(%subst(
062600141028     C                             %subst(vlrppt:posDaDft+1:
062700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800141028     C                   ENDIF
062900170214     C*
063000170214     C***
063100170214     C* Campi "personalizzati"
063200170214     C***
063300170214     C* PKN
063400170214     C                   IF        %subst(
063500170214     C                             %subst(vlrppt:posDaDft+1:
063600170214     C                             posADft-posDaDft-1):1:3)
063700170214     C                             = 'PKN'
063800170214     C                   EVAL      PiStr=%trim(%subst(
063900170214     C                             %subst(vlrppt:posDaDft+1:
064000170214     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100170214     C                   EXSR      CHKNUM
064200170214     C                   IF        PiNum=*on
064300170214     C                   Z-ADD     PiVal         wVABPKN           7 1
064400170214     C                   ENDIF
064500170214     C                   ENDIF
064600170214     C***
064700141028     C* ...
064800141028     C                   ENDIF
064900141028     C                   ENDDO
065000141028     C*
065100141028     C                   ENDSR
065200000801     C*----------------------------------------------------*
065300180115     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB EDIVAT EDIVAD EDIVAD10)
065400000801     C*----------------------------------------------------*
065500171218     C     IMPVABVAD     BEGSR
065600040910     C*
065700040910     C* Traduzione relativa ai tipi record del file del cliente
065800040910     C*
065900171108     C                   Z-ADD     0             wRecErr
066000071210     C*
066100071210     C***
066200141028     C* ...tipo record 'UNB' (info CMR)
066300141028     C                   EVAL      wTag = 'UNB'
066400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066500141028     C                   MOVEL     *blanks       savCMR           35
066600141028     C                   EVAL      InStringa =
066700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
066800180111     C                   EVAL      InSepara = '+'
066900141028     C                   EXSR      SR_SPLIT
067000141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
067100140922     C                                      %char(datcor) + ' ' + %char(oracor)
067200130620     C                   ENDIF
067300141028     C*
067400141028     C***
067500161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
067600161025     C                   EVAL      wTag = 'UNH'
067700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067800150526     C*
067900150526     C* Inizializzazioni per nuova spedizione
068000141028     C                   SETOFF                                       31
068100141028     C                   eval      vinflg = '1'
068200141028     C* ......inizializzazioni iniziali e formati record file Bartolini
068300141028     C                   EXSR      INZVAR
068400141028     C* ......valorizzazione campi fissi
068500141028     C                   MOVEL     datcor        VABAAS
068600141028     C                   MOVE      datcor        VABMGS
068700141028     C                   MOVE(P)   vlrpoi        VABFGS
068800141028     C                   EVAL      VABCMR = savCMR
068900141028     C                   EVAL      VABDCM = datcor
069000141028     C                   EVAL      VABDTS = datcor
069100141028     C                   EVAL      VABCNT = 1
069200141028     C* ......NSP => Stacco un numeratore da AZNUM
069300141028     C                   clear                   TRUL33DS
069400141028     C                   eval      I33OPE = *zeros
069500141028     C                   eval      I33CNU = 302
069600141028     C                   eval      I33NUM = 1
069700141028     C                   movel     TRUL33DS      KPJBU
069800141028     C                   call      'TRUL33R'
069900141028     C                   parm                    KPJBA
070000141028     C                   movel     KPJBU         TRUL33DS
070100141028     C                   if        O33ERR = *zeros
070200141028     C                   z-add     O33NRF        VABNSP
070300141028     C                   else
070400141028     C                   Z-ADD     1             wRecErr
070500141028     C                   SETON                                        31
070600141028     C                   EVAL      vinmsg = %trimr(vinmsg)
070700141028     C                             + ' ' + 'VABNSP VATNSP'
070800141028     C                   endif
070900141028     C                   ENDIF
071000161019     C*
071100161019     C***
071200170208     C* ...tipo record 'BGM' (riferimento spedizione)
071300170208     C                   EVAL      wTag = 'BGM+610'
071400161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071500161019     C                   EVAL      InStringa =
071600161019     C                                %subst(wvindta:%len(%trim(wTag))+2)
071700161025     C                   EVAL      InSepara = '+'
071800161019     C                   EXSR      SR_SPLIT
071900170208     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
072000161019     C                   ENDIF
072100150527     C*
072200150527     C***
072300171031     C* ...tipo record 'DTM+2' (data consegna richiesta)
072400171031     C                   EVAL      wTag = 'DTM+2'
072500150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072600150527     C                   EVAL      PiStr=%subst(
072700150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
072800150527     C                   EXSR      CHKNUM
072900150527     C                   IF        PiInt=*on
073000150527     C                   Z-ADD     PiVal         VABDCR
073100150527     C                   ELSE
073200150527     C                   Z-ADD     1             wRecErr
073300180111     C                   SETON                                        31
073400180111     C                   EVAL      vinmsg = %trimr(vinmsg)
073500180111     C                             + ' ' + 'VABDCR'
073600150527     C                   ENDIF
073700150527     C                   ENDIF
073800161025     C*
073900161025     C***
074000161025     C* ...tipo record 'MOA+22' (contrassegno / divisa)
074100170208     C***                EVAL      wTag = 'MOA+22'
074200170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074300170208     C***                EVAL      wFlgCAS = '1'
074400170208     C***                EVAL      InStringa =
074500170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
074600170208     C***                EVAL      InSepara = ':'
074700170208     C***                EXSR      SR_SPLIT
074800170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
074900170208     C***                EXSR      CHKNUM
075000170208     C***                IF        PiNum=*on
075100170208     C***                ADD       PiVal         VABCAS
075200170208     C***                EVAL      VABVCA = %trim(SkSplitCSV_5(2))
075300170208     C***                ELSE
075400170208     C***                Z-ADD     1             wRecErr
075500170208     C***                ENDIF
075600170208     C***                ENDIF
075700161025     C*
075800161025     C***
075900171031     C* ...tipo record 'FTX+DEL' (note di consegna)
076000171122     C                   EVAL      wTag = 'FTX+DEL'
076100171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076200171031     C                   EVAL      InStringa =
076300171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
076400171031     C                   EVAL      InSepara = '+'
076500171031     C                   EXSR      SR_SPLIT
076600171031        // LK: se voglio usare il Delivery Instruction, (forse) la pos. 3 non è quella giusta
076700171031     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
076800171031     C                                     %trim(SkSplitCSV_5(3))
076900171031     C                   ENDIF
077000171218     C*
077100171218     C***
077200171218     C* ...tipo record 'FTX+DIN' (particolarità varie / prenotazione telefonica)
077300171219     C                   EVAL      wTag = 'FTX+DIN'
077400171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077500171218     C                   EVAL      InStringa =
077600171218     C                                %subst(wvindta:%len(%trim(wTag))+2)
077700171218     C                   EVAL      InSepara = '+'
077800171218     C                   EXSR      SR_SPLIT
077900171218     C                   SELECT
078000171218     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'U'
078100171218     C                   EVAL      VABGVA = %trim(SkSplitCSV_5(3))
078200171218     C                   WHEN      %trim(SkSplitCSV_5(3)) <> *blank
078300171219     C* richiesto appuntamento
078400171219     C                   EVAL      VABTC1 = 'A'
078500171219     C* telefono
078600171218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
078700171219     C                   EVAL      VATTRC = 'B'
078800171218     C                   EXSR      WRIVAT
078900171218     C                   ENDSL
079000171218     C                   ENDIF
079100171031     C*
079200171031     C***
079300171031     C* ...tipo record 'NAD+CZ' (mittente originale)
079400171031     C                   EVAL      wTag = 'NAD+CZ'
079500171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079600171031     C                   EVAL      InStringa =
079700171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
079800171031     C                   EVAL      InSepara = '+'
079900171031     C                   EXSR      SR_SPLIT
080000171031     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
080100171031     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
080200171031     C                   IF        VABNMO = 'I'   OR
080300171031     C                             VABNMO = 'IT'  OR
080400171031     C                             VABNMO = 'ITA'
080500171031     C                   EVAL      VABNMO = *blanks
080600171031     C                   ELSE
080700171031     C                   Z-ADD     1             jNAZ
080800171031     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
080900171031     C                   IF        %found
081000171031     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
081100171031     C                   ENDIF
081200171031     C                   ENDIF
081300171031     C*
081400171031     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
081500171031     C                   EVAL      InSepara = ':'
081600171031     C                   EXSR      SR_SPLIT
081700171031     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
081800171031     C                   ENDIF
081900171031     C*
082000171218     C***
082100171031     C* ...tipo record 'NAD+CN' (destinatario)
082200171031     C                   EVAL      wTag = 'NAD+CN'
082300171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082400171031     C                   EVAL      InStringa =
082500171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
082600171031     C                   EVAL      InSepara = '+'
082700171031     C                   EXSR      SR_SPLIT
082800171031     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
082900171031     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
083000171122     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(6))
083100171122     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(7))
083200171031     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
083300171031     C                   IF        VABNZD = 'I'   OR
083400171031     C                             VABNZD = 'IT'  OR
083500171031     C                             VABNZD = 'ITA'
083600171031     C                   EVAL      VABNZD = *blanks
083700171031     C                   ENDIF
083800171218
083900171218     C* sono i dati CCM - LNA - ZNC
084000171218     C*   ancora non ho capito dove li mette Difarco, per cui imposto qualcosa sapendo che sarà
084100171218     C*   cambiata
084200171218     C* siccome sono dati nunmerici controllo che non ci siano vaslori alfanumerici
084300171218     C                   MONITOR
084400171219     C                   EVAL      VABCCM = %dec(%subst(SkSplitCSV_5(9): 1 : 7)
084500171219     C                                      : 7 : 0)
084600171219     C                   EVAL      VABLNA = %dec(%subst(SkSplitCSV_5(9): 24 : 3)
084700171219     C                                      : 3 : 0)
084800171219     C                   EVAL      VABZNC = %dec(%subst(SkSplitCSV_5(9): 27 : 2)
084900171219     C                                      : 2 : 0)
085000171218     C                   ON-ERROR
085100171218     C                   EVAL      VABCCM = 0
085200171218     C                   EVAL      VABLNA = 0
085300171218     C                   EVAL      VABZNC = 0
085400171218     C                   ENDMON
085500171218
085600171031     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
085700171031     C                   EVAL      InSepara = ':'
085800171031     C                   EXSR      SR_SPLIT
085900171031     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
086000171031     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
086100171031     C                   ENDIF
086200161025     C*
086300161025     C***
086400161025     C* ...tipo record 'CNT+7' (peso spedizione)
086500170208     C***                EVAL      wTag = 'CNT+7'
086600170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086700170208     C***                EVAL      InStringa =
086800170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
086900170208     C***                EVAL      InSepara = ':'
087000170208     C***                EXSR      SR_SPLIT
087100170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
087200170208     C***                EXSR      CHKNUM
087300170208     C***                IF        PiNum=*on
087400170208     C***                Z-ADD     PiVal         VABPKB
087500170208     C***                ELSE
087600170208     C***                Z-ADD     1             wRecErr
087700170208     C***                ENDIF
087800170208     C***                ENDIF
087900161025     C*
088000161025     C***
088100161025     C* ...tipo record 'CNT+15' (volume spedizione)
088200170208     C***                EVAL      wTag = 'CNT+15'
088300170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088400170208     C***                EVAL      InStringa =
088500170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
088600170208     C***                EVAL      InSepara = ':'
088700170208     C***                EXSR      SR_SPLIT
088800170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
088900170208     C***                EXSR      CHKNUM
089000170208     C***                IF        PiNum=*on
089100170208     C***                Z-ADD     PiVal         VABVLB
089200170208     C***                ELSE
089300170208     C***                Z-ADD     1             wRecErr
089400170208     C***                ENDIF
089500170208     C***                ENDIF
089600161025     C*
089700161025     C***
089800171108     C* ...tipo record 'GID+' (numero colli)
089900171218     C* E' il numero dei colli della singola delivery in caso di spedizione a colli/ 1 nel caso di
090000171218     C* pallet groupage
090100171108     C                   EVAL      wTag = 'GID+'
090200170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090300170209     C                   EVAL      InStringa =
090400180110     C                                %subst(wvindta:%len(%trim(wTag))+1)
090500180110     C* così ottengo una schiera di 2 elementi con 1) nr.progressivo 2) nr. colli+altro da splittare
090600180110     C                   EVAL      InSepara = '+'
090700170209     C                   EXSR      SR_SPLIT
090800180110     C                   EVAL      InStringa =
090900180110     C                                %trim(SkSplitCSV_5(2))
091000180110     C* così ottengo una schiera di 2 elementi con 1) nr.colli 2) tipologia colli
091100180110     C                   EVAL      InSepara = ':'
091200180110     C                   EXSR      SR_SPLIT
091300170209     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
091400170209     C                   EXSR      CHKNUM
091500170209     C                   IF        PiInt=*on
091600171218     C                   Z-ADD     PiVal         VABNCL
091700171218     C***                ADD       PiVal         VABNCL
091800170209     C                   ELSE
091900170209     C                   Z-ADD     1             wRecErr
092000180111     C                   SETON                                        31
092100180111     C                   EVAL      vinmsg = %trimr(vinmsg)
092200180111     C                             + ' ' + 'VABNCL'
092300170209     C                   ENDIF
092400170209     C                   ENDIF
092500171031     C*
092600171031     C***
092700180111     C* ...tipo record 'MEA+WT++KGM' (peso spedizione)
092800180111     C                   EVAL      wTag = 'MEA+WT++KGM'
092900171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093000171031     C                   EVAL      InStringa =
093100171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
093200171031     C                   EVAL      InSepara = ':'
093300171031     C                   EXSR      SR_SPLIT
093400171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
093500171031     C                   EXSR      CHKNUM
093600171031     C                   IF        PiNum=*on
093700171218     C                   Z-ADD     PiVal         VABPKB
093800171031     C                   ELSE
093900171031     C                   Z-ADD     1             wRecErr
094000180111     C                   SETON                                        31
094100180111     C                   EVAL      vinmsg = %trimr(vinmsg)
094200180111     C                             + ' ' + 'VABPKB'
094300171031     C                   ENDIF
094400171031     C                   ENDIF
094500171031     C*
094600171031     C***
094700171031     C* ...tipo record 'MEA+VOL++MTQ' (volume spedizione)
094800171108     C***                EVAL      wTag = 'MEA+VOL++MTQ'
094900171108     C                   EVAL      wTag = 'MEA+VOL+AAE+MTQ'
095000171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095100171031     C                   EVAL      InStringa =
095200171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
095300171031     C                   EVAL      InSepara = ':'
095400171031     C                   EXSR      SR_SPLIT
095500171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
095600171031     C                   EXSR      CHKNUM
095700171031     C                   IF        PiNum=*on
095800171218     C                   Z-ADD     PiVal         VABVLB
095900171031     C                   ELSE
096000171031     C                   Z-ADD     1             wRecErr
096100180111     C                   SETON                                        31
096200180111     C                   EVAL      vinmsg = %trimr(vinmsg)
096300180111     C                             + ' ' + 'VABVLB'
096400171031     C                   ENDIF
096500171031     C                   ENDIF
096600170209     C*
096700170209     C***
096800180111     C* ...tipo record 'RFF+CU' (delivery adidas)
096900180111     C                   EVAL      wTag = 'RFF+CU'
097000171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097100171218     C                   EVAL      wRFF += 1
097200171218     C                   EVAL      SkRFF(wRFF) =
097300171218     C                               %subst(wvindta:%len(%trim(wTag))+2)
097400171218     C                   ENDIF
097500170209     C*
097600170209     C***
097700180111     C* ...tipo record 'RFF+CR' (riferimento spedizione)
097800180111     C***                EVAL      wTag = 'RFF+CR'
097900171031     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
098000171031     C***                EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
098100171031     C***                EXSR      CHKNUM
098200171031     C***                IF        PiInt=*on
098300171031     C***                Z-ADD     PiVal         VABRMN
098400171031     C***                ELSE
098500171031     C***                EVAL      VABRMN = 1
098600171031     C***                Z-ADD     1             wRecErr
098700171031     C***                ENDIF
098800171031     C***                ENDIF
098900141028     C*
099000141028     C***
099100161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
099200171220     C* NOTA: questo traduttore tratta 2 casi:
099300171220     C* 1 = Sfusi: 1 sped. BRT (segm. UNH), 1 delivery adidas (segm. RFF), ed N coppie HU/segnacolli
099400171220     C*     (segm. GIN).
099500171220     C* 2 = Groupage: 1 sped. BRT (segm. UNH), N delivery adidas (segm. RFF), e 1 coppia
099600171220     C*     HU/segnacolli (segm. GIN).
099700171220     C* Nel segm. GIN ci sono massimo 5 coppie, il primo valore è BRT il secondo è adidas
099800171220     C* impostate così: GIN+BJ+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS'
099900171220     C* Quindi OGNI VOLTA che ho determinato un coppia, posso scrivere VAD10 perché:
100000171220     C* - nel caso 1 ho un solo RFF (che ho già recuperato) associato a tutte le coppie
100100171220     C* - nel caso 2 ho N RFF (che ho già recuperato) associati all'unica coppie
100200161013     C                   EVAL      wTag = 'GIN+BJ'
100300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100400150526     C                   EVAL      InStringa =
100500150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
100600171220     C                   EVAL      InSepara = '+'
100700171220     C                   EXSR      SR_SPLIT
100800171220     C                   EVAL      SkSplitCSV_Sav = SkSplitCSV_5
100900171220     C                   FOR       wI = 1 to 5
101000171220     C                   IF        %trim(SkSplitCSV_Sav(wI)) <> *blanks
101100171220     C                   EVAL      InStringa =
101200180111     C                             %trim(SkSplitCSV_Sav(wI))
101300171220     C                   EVAL      InSepara = ':'
101400171220     C                   EXSR      SR_SPLIT
101500180115     C                   EVAL      H_VADNCD = %dec(%trim(SkSplitCSV_5(1)):7:0)
101600180115     C                   EVAL      VADNCD = H_VADNCD
101700180115     C                   EVAL      H_VADUN1 = %trim(SkSplitCSV_5(2))
101800171220     C                   EXSR      WRIVAD1
101900171220     C                   ELSE
102000171220     C                   LEAVE
102100150526     C                   ENDIF
102200171220     C                   ENDFOR
102300171108     C                   ENDIF
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
105300141028     C  N31              ADD       1             §CTROKVB
105400141028     C  N31              exsr      CHKWRI
105500140117     C  N31              WRITE     EDIVAB00
105600150526     C                   ENDIF
105700010202     C*
105800000801     C* Ebbene...
105900000801     C                   ADD       1             §CTRMO
106000141028     C                   IF        wRecErr <> *zeros
106100000801     C                   ADD       1             §CTRNO
106200000801     C                   EVAL      vinflg = '2'
106300000801     C                   ENDIF
106400000801     C*
106500000801     C                   ENDSR
106600010202     C*----------------------------------------------------*
106700171219     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
106800010202     C*----------------------------------------------------*
106900171219     C     PREVAD1T      BEGSR
107000010202     C*
107100171219     C* Compongo il nome del membro da dare al EDIVATWR
107200010202     C                   eval      parmbr = vlrhdl
107300010202     C                   movel     'M'           parmbr
107400060113     C                   eval      parccm = vlrksc
107500010202     C                   eval      paropz = '1'
107600010202     C* Effettuo la chiamata al CLLE preposto
107700180116     C                   call(e)   'TITVEVD1TC'
107800180116     C***lk              call(e)   'TITVEVDTC'
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
112800171220     C*  SCRITTURA BUFFER EDIVAD10F
112900141028     C*----------------------------------------------------*
113000171220     C     WRIVAD1       BEGSR
113100180115     C*
113200180115     C                   EVAL      VADFGS = VABFGS
113300180115     C                   EVAL      VADCCM = VABCCM
113400180115     C                   EVAL      VADLNP = VABLNP
113500180115     C                   EVAL      VADAAS = VABAAS
113600180115     C                   EVAL      VADNRS = VABNRS
113700180115     C                   EVAL      VADNSP = VABNSP
113800180115     C                   EVAL      VADCMR = VABCMR
113900180115     C                   EVAL      VADCNT = VABCNT
114000180116     C                   EVAL      VADNCL = 1
114100180116     C                   ADD       1             §CTROKVD
114200180115     C                   WRITE     EDIVAD00
114300141028     C*
114400180115     C                   IF        H_VADUN1 <> *blanks
114500171220     C                   EVAL      wJ = 0
114600171220     C* devo scrivere tanti rcd quante sono le delivery adidas memorizzate nella schiera SkRFF
114700171220     C* che però è a dimensione variabile
114800171220     C                   DO        *hival
114900171220     C                   EVAL      wJ += 1
115000180116     C                   IF        SkRFF(wJ) = *blank
115100171220     C                   LEAVE
115200171220     C                   ENDIF
115300180115     C*
115400180115     C                   EVAL      H_VADFGS = VABFGS
115500180115     C                   EVAL      H_VADCCM = VABCCM
115600180115     C                   EVAL      H_VADLNP = VABLNP
115700180115     C                   EVAL      H_VADAAS = VABAAS
115800180115     C                   EVAL      H_VADNRS = VABNRS
115900180115     C                   EVAL      H_VADNSP = VABNSP
116000180115     C                   EVAL      H_VADCMR = VABCMR
116100180115     C                   EVAL      H_VADCNT = VABCNT
116200180116     C                   EVAL      H_VADNCL = 1
116300180115     C                   EVAL      H_VADUN2 = SkRFF(wJ)
116400180116     C                   WRITE     EDIVAD100
116500180116     C                   ENDDO
116600171220     C                   ENDIF
116700141028     C*
116800141028     C                   ENDSR
116900000801
117000171219
117100171219     C*----------------------------------------------------*
117200171219     C*  SCRITTURA BUFFER EDIVAT
117300171219     C*----------------------------------------------------*
117400171219     C     WRIVAT        BEGSR
117500171219     C*
117600171219     C                   IF        VATNOT <> *blanks
117700171219     C                   EVAL      VATFGS = VABFGS
117800171219     C                   EVAL      VATCCM = VABCCM
117900171219     C                   EVAL      VATLNP = VABLNP
118000171219     C                   EVAL      VATAAS = VABAAS
118100171219     C                   EVAL      VATNRS = VABNRS
118200171219     C                   EVAL      VATNSP = VABNSP
118300171219     C                   EVAL      VATCMR = VABCMR
118400171219     C                   EVAL      VATCNT = VABCNT
118500171219     C                   ADD       1             §CTROKVT
118600171219     C                   WRITE     EDIVAT00
118700171219     C                   ENDIF
118800171219     C*
118900171219     C                   ENDSR
119000171219
119100011113
119200011113     C*----------------------------------------------------*
119300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
119400011113     C*----------------------------------------------------*
119500011113     C     CHKIMPDIV     BEGSR
119600011113     C*
119700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
119800011113     C                   Z-ADD     *zeros        wrkDec            9 9
119900011113     C*
120000011113     C* Come prima cosa effettuo considerazioni sulla divisa
120100011113     C                   IF        vabIAS > *zeros
120200011113     C                   IF        vabVAS <> 'EUR'
120300011113     C                   EVAL      vabVAS =  'ITL'
120400011113     C                   ENDIF
120500011113     C                   ENDIF
120600011113     C*
120700011113     C                   IF        vabCAS > *zeros
120800011113     C                   IF        vabVCA <> 'EUR'
120900011113     C                   EVAL      vabVCA =  'ITL'
121000011113     C                   ENDIF
121100011113     C                   ENDIF
121200011113     C*
121300011113     C                   IF        vabVMD > *zeros
121400020305     C                   IF        vabVAD <> 'EUR'
121500011113     C                   EVAL      vabVAD =  'ITL'
121600011113     C                   ENDIF
121700011113     C                   ENDIF
121800011113     C*
121900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
122000011113     C                   Z-ADD     vabIAS        wrkDec
122100011113     C                   IF        wrkDec > *zeros
122200011113     C                   IF        vabVAS = 'ITL'
122300011113     C                   EVAL      vabIAS = *zeros
122400011113     C                   ENDIF
122500011113     C                   ENDIF
122600011113     C*
122700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
122800011113     C                   Z-ADD     vabCAS        wrkDec
122900011113     C                   IF        wrkDec > *zeros
123000011113     C                   IF        vabVCA = 'ITL'
123100011113     C                   EVAL      vabCAS = *zeros
123200011113     C                   ENDIF
123300011113     C                   ENDIF
123400011113     C*
123500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
123600011113     C                   Z-ADD     vabVMD        wrkDec
123700011113     C                   IF        wrkDec > *zeros
123800011113     C                   IF        vabVAD = 'ITL'
123900011113     C                   EVAL      vabVMD = *zeros
124000011113     C                   ENDIF
124100011113     C                   ENDIF
124200011113     C*
124300011113     C                   ENDSR
124400011113     C***
124500011113
124600011113
124700000801
124800000801
124900990920      /TITLE Invio dei dati al punto operativo.
125000010202     C     invio         BEGSR
125100990920     C*
125200171218     C* 1° invio EDIVAD
125300010201     C                   reset                   dscmz
125400010201     C                   move      vlrpoi        cmzdst
125500180115     C                   eval      cmzfld = 'EDIVADWR'
125600010201     C                   eval      cmzmbd = vlrhdl
125700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
125800021009     C***                if        prmfir = *blanks
125900180115     C                   eval      cmzfla = 'EDIVAD0F'
126000180115     C                   eval      cmzmba = 'EDIVAD0F'
126100021009     C***                else
126200021009     C***                eval      cmzfla = prmfir
126300021009     C***                eval      cmzmba = prmfir
126400021009     C***                endif
126500010201     C                   eval      cmznrr = *zeros
126600180116     C                   move      §ctrokvd      cmznrr
126700021018     C                   eval      cmzlba = vlrfl1
126800180116     C                   call(e)   'TIS711C'
126900180116     C                   parm                    dscmz
127000180116     C                   parm      *blanks       esito
127100010205     C                   if        %error
127200010205     C                             or cmzerr = '1'
127300010205     C                             or esito  = '1'
127400010205     C                   eval      wrkesito = '3'
127500010205     C                   else
127600180116     C*
127700180116     C* 1°bis invio EDIVAD10
127800180116     C                   reset                   dscmz
127900180116     C                   move      vlrpoi        cmzdst
128000180116     C                   eval      cmzfld = 'EDIVAD1WR'
128100180116     C                   eval      cmzmbd = vlrhdl
128200180116     C                   eval      %subst(cmzmbd:1:1) = 'M'
128300180116     C***                if        prmfir = *blanks
128400180116     C                   eval      cmzfla = 'EDIVAD10F'
128500180116     C                   eval      cmzmba = 'EDIVAD10F'
128600180116     C***                else
128700180116     C***                eval      cmzfla = prmfir
128800180116     C***                eval      cmzmba = prmfir
128900180116     C***                endif
129000180116     C                   eval      cmznrr = *zeros
129100180116     C                   move      §ctrokvd      cmznrr
129200180116     C                   eval      cmzlba = vlrfl1
129300180116     C                   call(e)   'TIS711C'
129400180116     C                   parm                    dscmz
129500180116     C                   parm      *blanks       esito
129600180116     C                   if        %error
129700180116     C                             or cmzerr = '1'
129800180116     C                             or esito  = '1'
129900180116     C                   eval      wrkesito = '3'
130000180116     C                   else
130100171219     C*
130200171219     C* 2° invio EDIVAT
130300171219     C                   reset                   dscmz
130400171219     C                   move      vlrpoi        cmzdst
130500171219     C                   eval      cmzfld = 'EDIVATWR'
130600171219     C                   eval      cmzmbd = vlrhdl
130700171219     C                   eval      %subst(cmzmbd:1:1) = 'M'
130800171219     C***                if        prmfir = *blanks
130900171219     C                   eval      cmzfla = 'EDIVAT0F'
131000171219     C                   eval      cmzmba = 'EDIVAT0F'
131100171219     C***                else
131200171219     C***                eval      cmzfla = prmfir
131300171219     C***                eval      cmzmba = prmfir
131400171219     C***                endif
131500171219     C                   eval      cmznrr = *zeros
131600171219     C                   move      §ctrokvt      cmznrr
131700171219     C                   eval      cmzlba = vlrfl1
131800171219     C                   call(e)   'TIS711C'
131900171219     C                   parm                    dscmz
132000171219     C                   parm      *blanks       esito
132100171219     C                   if        %error
132200171219     C                             or cmzerr = '1'
132300171219     C                             or esito  = '1'
132400171219     C                   eval      wrkesito = '3'
132500171219     C                   else
132600010201     C*
132700171219     C* 3° invio EDIVAB
132800010201     C                   reset                   dscmz
132900010201     C                   move      vlrpoi        cmzdst
133000010201     C                   eval      cmzfld = vlrfou
133100010201     C                   eval      cmzmbd = vlrhdl
133200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
133300021009     C***                if        prmfir = *blanks
133400140117     C                   eval      cmzfla = 'EDIVAB0F'
133500140117     C                   eval      cmzmba = 'EDIVAB0F'
133600021009     C***                else
133700021009     C***                eval      cmzfla = prmfir
133800021009     C***                eval      cmzmba = prmfir
133900021009     C***                endif
134000010201     C                   eval      cmznrr = *zeros
134100010201     C                   move      §ctrokvb      cmznrr
134200021018     C                   eval      cmzlba = vlrfl1
134300010201     C                   call(e)   'TIS711C'
134400010201     C                   parm                    dscmz
134500010201     C                   parm      *blanks       esito
134600010201     C                   if        %error
134700010201     C                             or cmzerr = '1'
134800010201     C                             or esito  = '1'
134900010201     C                   eval      wrkesito = '3'
135000010201     C                   endif
135100180116     C                   endif
135200010205     C                   endif
135300171219     C                   endif
135400990920     C*
135500000613     C                   ENDSR
135600000613     C***
135700070411
135800141028
135900070411     C     *pssr         BEGSR
136000070411     C*
136100070411     C                   if        %open(tivin00r)
136200070411     C                   close     tivin00r
136300070411     C                   endif
136400140117     C                   if        %open(edivabwr)
136500140117     C                   close     edivabwr
136600070411     C                   endif
136700180116     C                   if        %open(EDIVAD1wr)
136800180116     C                   close     EDIVAD1wr
136900180116     C                   endif
137000180115     C                   if        %open(EDIVADwr)
137100180115     C                   close     EDIVADwr
137200171219     C                   endif
137300180115     C                   if        %open(EDIVATwr)
137400180115     C                   close     EDIVATwr
137500180115     C                   endif
137600070411     C*
137700070411     C* Effettuo la chiamata al CLLE preposto
137800180116     C                   call(e)   'TITVEVD1TC'
137900180116     C***lk              call(e)   'TITVEVDTC'
138000070411     C                   parm                    parccm
138100070411     C                   parm                    parmbr
138200070411     C                   parm      '2'           paropz
138300070411     C*
138400070411     C                   eval      wrkesito = '2'
138500070411     C*
138600070411     C                   seton                                        LR
138700070411     C*
138800070411     C                   ENDSR     '*CANCL'
138900070411     C***
139000161018
139100161018
139200161018     C*--------------------------------------------------------
139300161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
139400161018     C*--------------------------------------------------------
139500161018     C     CARTAB        BEGSR
139600161018     C*
139700161018     C* TABELLA '15' - NAZIONI
139800161018     C                   clear                   skNAZISO
139900161018     C                   clear                   skNAZBAR
140000161018     C                   eval      tblKUT = 1
140100161018     C                   eval      tblCOD = '15'
140200161018     C     KEYtabP       setll     tabel00f
140300161018     C     KEYtabP       reade     tabel00f
140400161018     C                   dow       not %eof(tabel00f)
140500161018     C                   if        tblFLG = *blanks
140600161018     C                   movel(p)  tblUNI        ds15
140700161018     C                   add       1             jNAZ
140800161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
140900161018     C                   eval      skNAZISO(jNAZ) = §15COD
141000161018     C                   endif
141100161018     C     KEYtabP       reade     tabel00f
141200161018     C                   enddo
141300161018     C*
141400161018     C                   ENDSR
141500161018     C***
141600070411
141700990910
141800000613     C     *inzsr        BEGSR
141900990910     C*
142000990910     C     *entry        plist
142100990920     C                   parm                    tivlrds
142200990921     C                   parm      wrkesito      esito
142300000724     C                   parm                    prmlit
142400000710     C                   parm                    prmfir
142500000613     C*
142600000830     C* CALCOLA LA DATA CORRENTE
142700140117     C                   time                    wn14             14 0
142800140117     C                   movel     wn14          oracor            6 0          *ORA
142900100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
143000100722     C                   eval      datcor = %dec(%date() : *ISO)
143100161018     C*
143200161018     C* Chiave su TABEL00F - parziale
143300161018     C     KEYtabP       KLIST
143400161018     C                   KFLD                    tblKUT
143500161018     C                   KFLD                    tblCOD
143600000830     C*
143700000613     C                   ENDSR
143800000613     C***
