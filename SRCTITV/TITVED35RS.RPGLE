000100180115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR/EDIVAD1WR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400161018     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140117     FEDIVABwr  O    E             DISK    usropn
000700171218     FEDIVATwr  O    E             DISK    usropn
000800180115     FEDIVAD1wr O    E             DISK    usropn prefix(H_)
000900180115     FEDIVADwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070719     D tisi95ds      e ds
001900140117     D edivabds      e ds                  extname(edivab0f)
002000161018     D ds15          e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000141028     D wTag            s            128    VARYING INZ
003100141028     D wRecErr         s              1  0 INZ(*zeros)
003200141028     D wFlgCAS         s              1    INZ(*blanks)
003300141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003400150527     D wNOTE           s             70    INZ(*blanks)
003500161013     D wEMAIL          s             70    INZ(*blanks)
003600141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003900141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
004000141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
004100141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
004200141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004300171108     D wI              s              3s 0
004400171220     D wJ              s              3s 0
004500171218     D wRFF            s              3  0 INZ(*zeros)
004600161018
004700161018     D*------------
004800161018     D jNAZ            s              5  0 INZ(*zeros)
004900161018     D skNAZISO        S              3    DIM(1000)
005000161018     D skNAZBAR        S              3    DIM(1000)
005100141028
005200141028     D*------------------
005300141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005400141028     D*------------------
005500141028     D CharNUM         S              1
005600141028     D CharSOS         S              1
005700141028     D posDaDft        S              3  0 INZ(*zeros)
005800141028     D posADft         S              3  0 INZ(*zeros)
005900141028     D wGiroDft        s              1  0 INZ(*zeros)
006000141028
006100141028
006200141028     D*------------------
006300141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006400141028     D*------------------
006500141028     D InStringa       S          65535A   VARYING                              (stringa input)
006600141028     D InSepara        S             10A                                        (separatore)
006700141028     D InTypeOut       S              1A                                        (tipo output)
006800141028     D wSkParam        S          65535A                                        (output)
006900141028     D OutErrore       S              1A                                        (flag errore)
007000141028     D OutMsg          S             80A                                        (messaggio errore)
007100141028     D SkSplitCSV_5    S            256    DIM(256)
007200171220     D SkSplitCSV_Sav  S            256    DIM(256)
007300171218     D SkRFF           DS            35    DIM(100) based(PtrRFF)
007400171218     D                                     qualified
007500141028
007600000830
007700041025     D*------------------
007800041025     D* DS REPERIMENTO NUMERATORE
007900041025     D*------------------
008000041025     D trul33ds      e ds                  inz
008100041025     D*------------------
008200041025     D* DS ARCHITETTURA
008300041025     D*------------------
008400041025     D kpjba         e ds                  inz
008500041025     D*------------------
008600990908
008700141028
008800141028     D*------------------
008900141028     D* LINKING A DEFINIZIONI ESTERNE
009000141028     D*------------------
009100141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300150609
009400141028
009500150609     D*-------------------
009600150609     D* COSTANTI
009700150609     D*-------------------
009800150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009900150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010000141028
010100010201
010200010201
010300000913     C                   reset                   rrnum
010400990921     C                   reset                   esito
010500990921     C                   reset                   wrkesito
010600000613     C*
010700161018     C                   EXSR      CARTAB                                       CARICA TABELLE
010800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010900070719     C*
011000070719     C* Esegue lancio TISI95R solo x chiusura
011100070719     C                   CLEAR                   TISI95DS
011200070719     C                   EVAL      I95TLA = 'C'
011300070719     C                   CALL      'TISI95R'
011400070719     C                   PARM                    TISI95DS
011500000613     C*
011600010202     C* Effettuo la chiamata al CLLE preposto
011700180115     C***                call(e)   'TITVEVD1TC'
011800180115     C                   call(e)   'TITVEVDTC'
011900140922     C                   parm                    parccm
012000140922     C                   parm                    parmbr
012100140922     C                   parm      '2'           paropz
012200170731     C*
012300170731     C                   if        *in53
012400170731     C                   call      'TIS7P2SER'
012500170731     C                   parm      'C'           pIn_Opz           1
012600170731     C                   parm                    tivlrds
012700170731     C                   parm                    EDIVABDS
012800170731     C                   parm      *blanks       pIn_CdIdAz        2
012900170731     C                   parm      *blanks       pIn_MaskPDF      50
013000170731     C                   parm      *blanks       pOut_Esito        1
013100170731     C                   endif
013200000616     C*
013300010201     C                   seton                                        LR
013400990908
013500000801
013600910830     C*--------------------------------------------------------
013700171219     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR EDIVATWR e EDIVAD1WR   *
013800910830     C*--------------------------------------------------------
013900040526     C     RWFILE        BEGSR
014000990910     C*
014100990914     C                   if        not %open(tivin00r)
014200990908     C                   open      tivin00r
014300990914     C                   endif
014400140117     C                   if        not %open(edivabwr)
014500140117     C                   open      edivabwr
014600171219     C                   endif
014700171219     C*
014800180115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR/EDIVADWR/EDIVAD1WR
014900171219     C                   exsr      prevaD1T
015000171219     C*
015100171219     C                   if        chkcall = '0'
015200171219     C*
015300171219     C                   if        not %open(EDIVAD1wr)
015400180115     C                   open      EDIVAD1wr
015500171219     C                   endif
015600180115     C                   if        not %open(EDIVAD1wr)
015700180115     C                   open      EDIVADwr
015800180115     C                   endif
015900171219     C                   if        not %open(EDIVATwr)
016000171219     C                   open      EDIVATwr
016100010201     C                   endif
016200990910     C*
016300010201     C                   clear                   §CTROKVB          5 0
016400020305     C                   clear                   §CTROKVT          5 0
016500000801     C                   clear                   §CTRMO            5 0
016600000801     C                   clear                   §CTRNO            5 0
016700141028     C*
016800141028     C* Inizializzazioni fuori ciclo
016900141028     C                   EXSR      INZVAR
017000140117     C*
017100140902     C                   if        *in53
017200140117     C                   call      'TIS7P2SER'
017300140117     C                   parm      'O'           pIn_Opz           1
017400140117     C                   parm                    tivlrds
017500140117     C                   parm                    EDIVABDS
017600140117     C                   parm      *blanks       pIn_CdIdAz        2
017700140117     C                   parm      *blanks       pIn_MaskPDF      50
017800140117     C                   parm      *blanks       pOut_Esito        1
017900140902     C                   endif
018000130708     C*
018100921023     C                   DO        *HIVAL
018200990913     C*
018300990915     C                   READ      tivin00r                               70
018400040910     C                   if        vindta > *blanks
018500000613     C                   add       1             rrnum
018600000801     C*
018700000801     C                   if        *in70 = *off
018800000801     C                             and
018900000801     C                             (vinflg = *blanks
019000000801     C                              or vinflg = '0'
019100000801     C                              or vinflg = '2')
019200000801     C*
019300000801     C                   clear                   vinmsg
019400141028     C*
019500141028     C* "normalizzo" il dato di input
019600141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
019700141028     C                                             %len(%trim(vindta))-1))
019800160209     C*
019900160209     C* Elimino le "duple di controllo EDIFACT"
020000160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
020100160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
020200160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
020300160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
020400160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
020500040910     C*
020600040910     C* Eseguo routine d traduzione
020700171218     C                   exsr      impvabvad
020800040802     C*
020900010305     C                   endif
021000000905     C*
021100000905     C                   else
021200000905     C                   eval      vinflg = '1'
021300000905     C                   endif
021400000905     C*
021500000905     C  N70              update    tivin000
021600000905     C*
021700991022     C  N70              ENDdo
021800100722     C*
021900100722     C* Scarico i buffer testata ancora "in canna"
022000140922     C*
022100141028     C***  N31              ADD       1             §CTROKVB
022200141028     C***  N31              exsr      CHKWRI
022300141028     C***  N31              WRITE     EDIVAB00
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200140117     C                   if        %open(edivabwr)
024300140117     C                   close     edivabwr
024400990914     C                   endif
024500171219     C                   if        %open(EDIVAD1wr)
024600171219     C                   close     EDIVAD1wr
024700010201     C                   endif
024800180115     C                   if        %open(EDIVADwr)
024900180115     C                   close     EDIVADwr
025000180115     C                   endif
025100171219     C                   if        %open(EDIVATwr)
025200171219     C                   close     EDIVATwr
025300171219     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200140922
026300140922     C*----------------------------------------------------*
026400140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
026500140922     C*----------------------------------------------------*
026600140922     C     CHKWRI        BEGSR
026700140922     C*
026800141028     C                   if        VABRMN = *zeros
026900141028     C                   eval      VABRMN = VABNSP
027000141028     C                   endif
027100170214     C*
027200171031     C* Se peso non indicato nei dati cliente e richiesto un peso forzato per collo
027300170214     C                   IF        VABPKB = *zeros
027400170214     C                   EVAL      VABPKB = wVABPKN * VABNCL
027500170214     C                   ENDIF
027600141028     C*
027700141028     C                   if        VABPRD = *blanks
027800141028     C* ......VABPRD
027900141028     C* Reperisco la provincia dal CAP e dalla località
028000141028     C                   IF        VABLOD <> *blanks AND
028100141028     C                             VABCAD <> *blanks AND
028200141028     C                             VABNZD  = *blanks
028300141028     C                   CLEAR                   TISI95DS
028400141028     C                   EVAL      I95TCN = '3'
028500141028     C                   Z-ADD     datcor        I95DAT
028600141028     C                   EVAL      I95CAP = VABCAD
028700141028     C                   EVAL      I95LOC = VABLOD
028800141028     C                   CALL      'TISI95R'
028900141028     C                   PARM                    TISI95DS
029000141028     C                   EVAL      VABPRD = O95PRV
029100141028     C                   ENDIF
029200141028     C                   endif
029300140922     C*
029400140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029500141028     C                   IF        wFlgCAS <> '0'
029600140922     C                   IF        VABCBO = '1'
029700140922     C                   EVAL      VABCBO = '4'
029800140922     C                   ELSE
029900140922     C                   EVAL      VABCBO = '6'
030000140922     C                   ENDIF
030100140922     C                   ENDIF
030200140922     C*
030300140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030400141028     C                   EXSR      CHKIMPDIV
030500150527     C*
030600150527     C* Scompongo le note
030700150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
030800150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
030900140922     C*
031000140922     C                   ENDSR
031100141028     C*----------------------------------------------------*
031200141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031300141028     C*----------------------------------------------------*
031400141028     C     INZVAR        BEGSR
031500141028     C*
031600141028     C* Inizializzo variabili di wrk
031700141028     C                   Z-ADD     *zeros        Num5_0            5 0
031800141028     C                   MOVEL     '0'           wFlgCAS
031900141028     C                   MOVEL     *blanks       wVATNOT_A
032000141028     C                   MOVEL     *blanks       wVATNOT_B
032100141028     C                   MOVEL     *blanks       wVATNOT_E
032200141028     C                   MOVEL     *blanks       wVATNOT_I
032300141028     C                   MOVEL     *blanks       wVATNOT_J
032400141028     C                   MOVEL     *blanks       wVATNOT_S
032500141028     C                   MOVEL     *blanks       wVATNOT_P
032600141028     C                   CLEAR                   SkSplitCSV_5
032700150527     C                   MOVEL     *blanks       wNOTE
032800171218     C                   EVAL      PtrRFF = %alloc(1)
032900141028     C*
033000141028     C* Reimposto i valori di default
033100141028     C                   EXSR      DEFCAM
033200141028     C*
033300141028     C                   ENDSR
033400141028     C*----------------------------------------------------*
033500141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
033600141028     C*----------------------------------------------------*
033700141028     C     DEFCAM        BEGSR
033800141028     C*
033900141028     C                   CLEAR                   EDIVAB00
034000171219     C                   CLEAR                   EDIVAT00
034100180115     C                   CLEAR                   EDIVAD00
034200171219     C                   CLEAR                   EDIVAD100
034300141028     C*
034400141028     C* Imposto i valori bolla di default
034500141028     C                   EVAL      VABCTR = 000
034600141028     C                   EVAL      VABCBO = '1'
034700141104     C                   EVAL      VABTSP = 'C'
034800141028     C*
034900141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035000141028     C* e delimitatore testo.
035100141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035200141028     C*
035300141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
035400141028     C                   EVAL      CharSOS = CharNUM
035500141028     C*
035600141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
035700141028     C                   SETOFF                                       53
035800141028     C                   SELECT
035900141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
036000141028     C                   SETON                                        53
036100141028     C                   ENDSL
036200141028     C*
036300141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
036400141028     C                   EVAL      posDaDft = 1
036500141028     C                   EVAL      posADft  = 0
036600141028     C                   EVAL      wGiroDft = 0
036700141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
036800141028     C                             posDaDft > 0
036900141028     C*
037000141028     C* Gestisco il 1° giro
037100141028     C                   IF        wGiroDft = 0
037200141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
037300141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
037400141028     C* Incremento il contatore dei "giri"
037500141028     C                   EVAL      wGiroDft = 1
037600141028     C                   ELSE
037700141028     C                   EVAL      posDaDft = posADft
037800141028     C                   ENDIF
037900141028     C* Eseguo lo scan x trovare la fine del campo corrente
038000141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
038100141028     C*
038200141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
038300141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
038400141028     C                   IF        posDaDft > 0 AND
038500141028     C                             posADft  > 0
038600141028     C* NCL
038700141028     C                   IF        %subst(
038800141028     C                             %subst(vlrppt:posDaDft+1:
038900141028     C                             posADft-posDaDft-1):1:3)
039000141028     C                             = 'NCL'
039100141028     C                   EVAL      PiStr=%trim(%subst(
039200141028     C                             %subst(vlrppt:posDaDft+1:
039300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039400141028     C                   EXSR      CHKNUM
039500141028     C                   IF        PiInt=*on
039600141028     C                   Z-ADD     PiVal         VABNCL
039700141028     C                   ENDIF
039800141028     C                   ENDIF
039900141028     C* CCM
040000141028     C                   IF        %subst(
040100141028     C                             %subst(vlrppt:posDaDft+1:
040200141028     C                             posADft-posDaDft-1):1:3)
040300141028     C                             = 'CCM'
040400141028     C                   EVAL      PiStr=%trim(%subst(
040500141028     C                             %subst(vlrppt:posDaDft+1:
040600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040700141028     C                   EXSR      CHKNUM
040800141028     C                   IF        PiInt=*on
040900171219     C                   Z-ADD     PiVal         VABCCM
041000141028     C                   ENDIF
041100141028     C                   ENDIF
041200141028     C* LNP
041300141028     C                   IF        %subst(
041400141028     C                             %subst(vlrppt:posDaDft+1:
041500141028     C                             posADft-posDaDft-1):1:3)
041600141028     C                             = 'LNP'
041700141028     C                   EVAL      PiStr=%trim(%subst(
041800141028     C                             %subst(vlrppt:posDaDft+1:
041900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042000141028     C                   EXSR      CHKNUM
042100141028     C                   IF        PiInt=*on
042200141028     C                   Z-ADD     PiVal         VABLNP
042300141028     C                   ENDIF
042400141028     C                   ENDIF
042500141028     C* NRS
042600141028     C                   IF        %subst(
042700141028     C                             %subst(vlrppt:posDaDft+1:
042800141028     C                             posADft-posDaDft-1):1:3)
042900141028     C                             = 'NRS'
043000141028     C                   EVAL      PiStr=%trim(%subst(
043100141028     C                             %subst(vlrppt:posDaDft+1:
043200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043300141028     C                   EXSR      CHKNUM
043400141028     C                   IF        PiInt=*on
043500141028     C                   Z-ADD     PiVal         VABNRS
043600141028     C                   ENDIF
043700141028     C                   ENDIF
043800141028     C* CTR
043900141028     C                   IF        %subst(
044000141028     C                             %subst(vlrppt:posDaDft+1:
044100141028     C                             posADft-posDaDft-1):1:3)
044200141028     C                             = 'CTR'
044300141028     C                   EVAL      PiStr=%trim(%subst(
044400141028     C                             %subst(vlrppt:posDaDft+1:
044500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044600141028     C                   EXSR      CHKNUM
044700141028     C                   IF        PiInt=*on
044800141028     C                   Z-ADD     PiVal         VABCTR
044900141028     C                   ENDIF
045000141028     C                   ENDIF
045100141028     C* PKB
045200141028     C                   IF        %subst(
045300141028     C                             %subst(vlrppt:posDaDft+1:
045400141028     C                             posADft-posDaDft-1):1:3)
045500141028     C                             = 'PKB'
045600141028     C                   EVAL      PiStr=%trim(%subst(
045700141028     C                             %subst(vlrppt:posDaDft+1:
045800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045900141028     C                   EXSR      CHKNUM
046000141028     C                   IF        PiNum=*on
046100141028     C                   Z-ADD     PiVal         VABPKB
046200141028     C                   ENDIF
046300141028     C                   ENDIF
046400141028     C* VLB
046500141028     C                   IF        %subst(
046600141028     C                             %subst(vlrppt:posDaDft+1:
046700141028     C                             posADft-posDaDft-1):1:3)
046800141028     C                             = 'VLB'
046900141028     C                   EVAL      PiStr=%trim(%subst(
047000141028     C                             %subst(vlrppt:posDaDft+1:
047100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200141028     C                   EXSR      CHKNUM
047300141028     C                   IF        PiNum=*on
047400141028     C                   Z-ADD     PiVal         VABVLB
047500141028     C                   ENDIF
047600141028     C                   ENDIF
047700141028     C* QFT
047800141028     C                   IF        %subst(
047900141028     C                             %subst(vlrppt:posDaDft+1:
048000141028     C                             posADft-posDaDft-1):1:3)
048100141028     C                             = 'QFT'
048200141028     C                   EVAL      PiStr=%trim(%subst(
048300141028     C                             %subst(vlrppt:posDaDft+1:
048400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500141028     C                   EXSR      CHKNUM
048600141028     C                   IF        PiNum=*on
048700141028     C                   Z-ADD     PiVal         VABQFT
048800141028     C                   ENDIF
048900141028     C                   ENDIF
049000141028     C* CBO
049100141028     C                   IF        %subst(
049200141028     C                             %subst(vlrppt:posDaDft+1:
049300141028     C                             posADft-posDaDft-1):1:3)
049400141028     C                             = 'CBO'
049500141028     C                   EVAL      VABCBO=%trim(%subst(
049600141028     C                             %subst(vlrppt:posDaDft+1:
049700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800141028     C                   ENDIF
049900141028     C* TSP
050000141028     C                   IF        %subst(
050100141028     C                             %subst(vlrppt:posDaDft+1:
050200141028     C                             posADft-posDaDft-1):1:3)
050300141028     C                             = 'TSP'
050400141028     C                   EVAL      VABTSP=%trim(%subst(
050500141028     C                             %subst(vlrppt:posDaDft+1:
050600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700141028     C                   ENDIF
050800141028     C* VAS
050900141028     C                   IF        %subst(
051000141028     C                             %subst(vlrppt:posDaDft+1:
051100141028     C                             posADft-posDaDft-1):1:3)
051200141028     C                             = 'VAS'
051300141028     C                   EVAL      VABVAS=%trim(%subst(
051400141028     C                             %subst(vlrppt:posDaDft+1:
051500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600141028     C                   ENDIF
051700141028     C* VCA
051800141028     C                   IF        %subst(
051900141028     C                             %subst(vlrppt:posDaDft+1:
052000141028     C                             posADft-posDaDft-1):1:3)
052100141028     C                             = 'VCA'
052200141028     C                   EVAL      VABVCA=%trim(%subst(
052300141028     C                             %subst(vlrppt:posDaDft+1:
052400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500141028     C                   ENDIF
052600141028     C* TIC
052700141028     C                   IF        %subst(
052800141028     C                             %subst(vlrppt:posDaDft+1:
052900141028     C                             posADft-posDaDft-1):1:3)
053000141028     C                             = 'TIC'
053100141028     C                   EVAL      VABTIC=%trim(%subst(
053200141028     C                             %subst(vlrppt:posDaDft+1:
053300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400141028     C                   ENDIF
053500141028     C* GCA
053600141028     C                   IF        %subst(
053700141028     C                             %subst(vlrppt:posDaDft+1:
053800141028     C                             posADft-posDaDft-1):1:3)
053900141028     C                             = 'GCA'
054000141028     C                   EVAL      VABGCA=%trim(%subst(
054100141028     C                             %subst(vlrppt:posDaDft+1:
054200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300141028     C                   ENDIF
054400141028     C* CTM
054500141028     C                   IF        %subst(
054600141028     C                             %subst(vlrppt:posDaDft+1:
054700141028     C                             posADft-posDaDft-1):1:3)
054800141028     C                             = 'CTM'
054900141028     C                   EVAL      VABCTM=%trim(%subst(
055000141028     C                             %subst(vlrppt:posDaDft+1:
055100171218     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200141028     C                   ENDIF
055300141028     C* FFD
055400141028     C                   IF        %subst(
055500141028     C                             %subst(vlrppt:posDaDft+1:
055600141028     C                             posADft-posDaDft-1):1:3)
055700141028     C                             = 'FFD'
055800141028     C                   EVAL      VABFFD=%trim(%subst(
055900141028     C                             %subst(vlrppt:posDaDft+1:
056000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100141028     C                   ENDIF
056200141028     C* VAD
056300141028     C                   IF        %subst(
056400141028     C                             %subst(vlrppt:posDaDft+1:
056500141028     C                             posADft-posDaDft-1):1:3)
056600141028     C                             = 'VAD'
056700141028     C                   EVAL      VABVAD=%trim(%subst(
056800141028     C                             %subst(vlrppt:posDaDft+1:
056900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000141028     C                   ENDIF
057100141028     C* GMA
057200141028     C                   IF        %subst(
057300141028     C                             %subst(vlrppt:posDaDft+1:
057400141028     C                             posADft-posDaDft-1):1:3)
057500141028     C                             = 'GMA'
057600141028     C                   EVAL      VABGMA=%trim(%subst(
057700141028     C                             %subst(vlrppt:posDaDft+1:
057800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900141028     C                   ENDIF
058000141028     C* GGA
058100141028     C                   IF        %subst(
058200141028     C                             %subst(vlrppt:posDaDft+1:
058300141028     C                             posADft-posDaDft-1):1:3)
058400141028     C                             = 'GGA'
058500141028     C                   EVAL      VABGGA=%trim(%subst(
058600141028     C                             %subst(vlrppt:posDaDft+1:
058700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800141028     C                   ENDIF
058900141028     C* GVA
059000141028     C                   IF        %subst(
059100141028     C                             %subst(vlrppt:posDaDft+1:
059200141028     C                             posADft-posDaDft-1):1:3)
059300141028     C                             = 'GVA'
059400141028     C                   EVAL      VABGVA=%trim(%subst(
059500141028     C                             %subst(vlrppt:posDaDft+1:
059600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700141028     C                   ENDIF
059800141028     C* TC1
059900141028     C                   IF        %subst(
060000141028     C                             %subst(vlrppt:posDaDft+1:
060100141028     C                             posADft-posDaDft-1):1:3)
060200141028     C                             = 'TC1'
060300141028     C                   EVAL      VABTC1=%trim(%subst(
060400141028     C                             %subst(vlrppt:posDaDft+1:
060500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600141028     C                   ENDIF
060700141028     C* TC2
060800141028     C                   IF        %subst(
060900141028     C                             %subst(vlrppt:posDaDft+1:
061000141028     C                             posADft-posDaDft-1):1:3)
061100141028     C                             = 'TC2'
061200141028     C                   EVAL      VABTC2=%trim(%subst(
061300141028     C                             %subst(vlrppt:posDaDft+1:
061400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500141028     C                   ENDIF
061600141028     C* VATTRC
061700141028     C                   IF        %subst(
061800141028     C                             %subst(vlrppt:posDaDft+1:
061900141028     C                             posADft-posDaDft-1):1:3)
062000141028     C                             = 'TRC'
062100141028     C                   EVAL      VATTRC=%trim(%subst(
062200141028     C                             %subst(vlrppt:posDaDft+1:
062300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400141028     C                   ENDIF
062500170214     C*
062600170214     C***
062700170214     C* Campi "personalizzati"
062800170214     C***
062900170214     C* PKN
063000170214     C                   IF        %subst(
063100170214     C                             %subst(vlrppt:posDaDft+1:
063200170214     C                             posADft-posDaDft-1):1:3)
063300170214     C                             = 'PKN'
063400170214     C                   EVAL      PiStr=%trim(%subst(
063500170214     C                             %subst(vlrppt:posDaDft+1:
063600170214     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700170214     C                   EXSR      CHKNUM
063800170214     C                   IF        PiNum=*on
063900170214     C                   Z-ADD     PiVal         wVABPKN           7 1
064000170214     C                   ENDIF
064100170214     C                   ENDIF
064200170214     C***
064300141028     C* ...
064400141028     C                   ENDIF
064500141028     C                   ENDDO
064600141028     C*
064700141028     C                   ENDSR
064800000801     C*----------------------------------------------------*
064900180115     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB EDIVAT EDIVAD EDIVAD10)
065000000801     C*----------------------------------------------------*
065100171218     C     IMPVABVAD     BEGSR
065200040910     C*
065300040910     C* Traduzione relativa ai tipi record del file del cliente
065400040910     C*
065500171108     C                   Z-ADD     0             wRecErr
065600071210     C*
065700071210     C***
065800141028     C* ...tipo record 'UNB' (info CMR)
065900141028     C                   EVAL      wTag = 'UNB'
066000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066100141028     C                   MOVEL     *blanks       savCMR           35
066200141028     C                   EVAL      InStringa =
066300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
066400180111     C                   EVAL      InSepara = '+'
066500141028     C                   EXSR      SR_SPLIT
066600141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
066700140922     C                                      %char(datcor) + ' ' + %char(oracor)
066800130620     C                   ENDIF
066900141028     C*
067000141028     C***
067100161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
067200161025     C                   EVAL      wTag = 'UNH'
067300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067400150526     C*
067500150526     C* Inizializzazioni per nuova spedizione
067600141028     C                   SETOFF                                       31
067700141028     C                   eval      vinflg = '1'
067800141028     C* ......inizializzazioni iniziali e formati record file Bartolini
067900141028     C                   EXSR      INZVAR
068000141028     C* ......valorizzazione campi fissi
068100141028     C                   MOVEL     datcor        VABAAS
068200141028     C                   MOVE      datcor        VABMGS
068300141028     C                   MOVE(P)   vlrpoi        VABFGS
068400141028     C                   EVAL      VABCMR = savCMR
068500141028     C                   EVAL      VABDCM = datcor
068600141028     C                   EVAL      VABDTS = datcor
068700141028     C                   EVAL      VABCNT = 1
068800141028     C* ......NSP => Stacco un numeratore da AZNUM
068900141028     C                   clear                   TRUL33DS
069000141028     C                   eval      I33OPE = *zeros
069100141028     C                   eval      I33CNU = 302
069200141028     C                   eval      I33NUM = 1
069300141028     C                   movel     TRUL33DS      KPJBU
069400141028     C                   call      'TRUL33R'
069500141028     C                   parm                    KPJBA
069600141028     C                   movel     KPJBU         TRUL33DS
069700141028     C                   if        O33ERR = *zeros
069800141028     C                   z-add     O33NRF        VABNSP
069900141028     C                   else
070000141028     C                   Z-ADD     1             wRecErr
070100141028     C                   SETON                                        31
070200141028     C                   EVAL      vinmsg = %trimr(vinmsg)
070300141028     C                             + ' ' + 'VABNSP VATNSP'
070400141028     C                   endif
070500141028     C                   ENDIF
070600161019     C*
070700161019     C***
070800170208     C* ...tipo record 'BGM' (riferimento spedizione)
070900170208     C                   EVAL      wTag = 'BGM+610'
071000161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071100161019     C                   EVAL      InStringa =
071200161019     C                                %subst(wvindta:%len(%trim(wTag))+2)
071300161025     C                   EVAL      InSepara = '+'
071400161019     C                   EXSR      SR_SPLIT
071500170208     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
071600161019     C                   ENDIF
071700150527     C*
071800150527     C***
071900171031     C* ...tipo record 'DTM+2' (data consegna richiesta)
072000171031     C                   EVAL      wTag = 'DTM+2'
072100150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072200150527     C                   EVAL      PiStr=%subst(
072300150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
072400150527     C                   EXSR      CHKNUM
072500150527     C                   IF        PiInt=*on
072600150527     C                   Z-ADD     PiVal         VABDCR
072700150527     C                   ELSE
072800150527     C                   Z-ADD     1             wRecErr
072900180111     C                   SETON                                        31
073000180111     C                   EVAL      vinmsg = %trimr(vinmsg)
073100180111     C                             + ' ' + 'VABDCR'
073200150527     C                   ENDIF
073300150527     C                   ENDIF
073400161025     C*
073500161025     C***
073600161025     C* ...tipo record 'MOA+22' (contrassegno / divisa)
073700170208     C***                EVAL      wTag = 'MOA+22'
073800170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073900170208     C***                EVAL      wFlgCAS = '1'
074000170208     C***                EVAL      InStringa =
074100170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
074200170208     C***                EVAL      InSepara = ':'
074300170208     C***                EXSR      SR_SPLIT
074400170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
074500170208     C***                EXSR      CHKNUM
074600170208     C***                IF        PiNum=*on
074700170208     C***                ADD       PiVal         VABCAS
074800170208     C***                EVAL      VABVCA = %trim(SkSplitCSV_5(2))
074900170208     C***                ELSE
075000170208     C***                Z-ADD     1             wRecErr
075100170208     C***                ENDIF
075200170208     C***                ENDIF
075300161025     C*
075400161025     C***
075500171031     C* ...tipo record 'FTX+DEL' (note di consegna)
075600171122     C                   EVAL      wTag = 'FTX+DEL'
075700171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075800171031     C                   EVAL      InStringa =
075900171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
076000171031     C                   EVAL      InSepara = '+'
076100171031     C                   EXSR      SR_SPLIT
076200171031        // LK: se voglio usare il Delivery Instruction, (forse) la pos. 3 non è quella giusta
076300171031     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
076400171031     C                                     %trim(SkSplitCSV_5(3))
076500171031     C                   ENDIF
076600171218     C*
076700171218     C***
076800171218     C* ...tipo record 'FTX+DIN' (particolarità varie / prenotazione telefonica)
076900171219     C                   EVAL      wTag = 'FTX+DIN'
077000171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077100171218     C                   EVAL      InStringa =
077200171218     C                                %subst(wvindta:%len(%trim(wTag))+2)
077300171218     C                   EVAL      InSepara = '+'
077400171218     C                   EXSR      SR_SPLIT
077500171218     C                   SELECT
077600171218     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'U'
077700171218     C                   EVAL      VABGVA = %trim(SkSplitCSV_5(3))
077800171218     C                   WHEN      %trim(SkSplitCSV_5(3)) <> *blank
077900171219     C* richiesto appuntamento
078000171219     C                   EVAL      VABTC1 = 'A'
078100171219     C* telefono
078200171218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
078300171219     C                   EVAL      VATTRC = 'B'
078400171218     C                   EXSR      WRIVAT
078500171218     C                   ENDSL
078600171218     C                   ENDIF
078700171031     C*
078800171031     C***
078900171031     C* ...tipo record 'NAD+CZ' (mittente originale)
079000171031     C                   EVAL      wTag = 'NAD+CZ'
079100171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079200171031     C                   EVAL      InStringa =
079300171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
079400171031     C                   EVAL      InSepara = '+'
079500171031     C                   EXSR      SR_SPLIT
079600171031     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
079700171031     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
079800171031     C                   IF        VABNMO = 'I'   OR
079900171031     C                             VABNMO = 'IT'  OR
080000171031     C                             VABNMO = 'ITA'
080100171031     C                   EVAL      VABNMO = *blanks
080200171031     C                   ELSE
080300171031     C                   Z-ADD     1             jNAZ
080400171031     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
080500171031     C                   IF        %found
080600171031     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
080700171031     C                   ENDIF
080800171031     C                   ENDIF
080900171031     C*
081000171031     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
081100171031     C                   EVAL      InSepara = ':'
081200171031     C                   EXSR      SR_SPLIT
081300171031     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
081400171031     C                   ENDIF
081500171031     C*
081600171218     C***
081700171031     C* ...tipo record 'NAD+CN' (destinatario)
081800171031     C                   EVAL      wTag = 'NAD+CN'
081900171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082000171031     C                   EVAL      InStringa =
082100171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
082200171031     C                   EVAL      InSepara = '+'
082300171031     C                   EXSR      SR_SPLIT
082400171031     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
082500171031     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
082600171122     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(6))
082700171122     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(7))
082800171031     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
082900171031     C                   IF        VABNZD = 'I'   OR
083000171031     C                             VABNZD = 'IT'  OR
083100171031     C                             VABNZD = 'ITA'
083200171031     C                   EVAL      VABNZD = *blanks
083300171031     C                   ENDIF
083400171218
083500171218     C* sono i dati CCM - LNA - ZNC
083600171218     C*   ancora non ho capito dove li mette Difarco, per cui imposto qualcosa sapendo che sarà
083700171218     C*   cambiata
083800171218     C* siccome sono dati nunmerici controllo che non ci siano vaslori alfanumerici
083900171218     C                   MONITOR
084000171219     C                   EVAL      VABCCM = %dec(%subst(SkSplitCSV_5(9): 1 : 7)
084100171219     C                                      : 7 : 0)
084200171219     C                   EVAL      VABLNA = %dec(%subst(SkSplitCSV_5(9): 24 : 3)
084300171219     C                                      : 3 : 0)
084400171219     C                   EVAL      VABZNC = %dec(%subst(SkSplitCSV_5(9): 27 : 2)
084500171219     C                                      : 2 : 0)
084600171218     C                   ON-ERROR
084700171218     C                   EVAL      VABCCM = 0
084800171218     C                   EVAL      VABLNA = 0
084900171218     C                   EVAL      VABZNC = 0
085000171218     C                   ENDMON
085100171218
085200171031     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
085300171031     C                   EVAL      InSepara = ':'
085400171031     C                   EXSR      SR_SPLIT
085500171031     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
085600171031     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
085700171031     C                   ENDIF
085800161025     C*
085900161025     C***
086000161025     C* ...tipo record 'CNT+7' (peso spedizione)
086100170208     C***                EVAL      wTag = 'CNT+7'
086200170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086300170208     C***                EVAL      InStringa =
086400170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
086500170208     C***                EVAL      InSepara = ':'
086600170208     C***                EXSR      SR_SPLIT
086700170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
086800170208     C***                EXSR      CHKNUM
086900170208     C***                IF        PiNum=*on
087000170208     C***                Z-ADD     PiVal         VABPKB
087100170208     C***                ELSE
087200170208     C***                Z-ADD     1             wRecErr
087300170208     C***                ENDIF
087400170208     C***                ENDIF
087500161025     C*
087600161025     C***
087700161025     C* ...tipo record 'CNT+15' (volume spedizione)
087800170208     C***                EVAL      wTag = 'CNT+15'
087900170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088000170208     C***                EVAL      InStringa =
088100170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
088200170208     C***                EVAL      InSepara = ':'
088300170208     C***                EXSR      SR_SPLIT
088400170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
088500170208     C***                EXSR      CHKNUM
088600170208     C***                IF        PiNum=*on
088700170208     C***                Z-ADD     PiVal         VABVLB
088800170208     C***                ELSE
088900170208     C***                Z-ADD     1             wRecErr
089000170208     C***                ENDIF
089100170208     C***                ENDIF
089200161025     C*
089300161025     C***
089400171108     C* ...tipo record 'GID+' (numero colli)
089500171218     C* E' il numero dei colli della singola delivery in caso di spedizione a colli/ 1 nel caso di
089600171218     C* pallet groupage
089700171108     C                   EVAL      wTag = 'GID+'
089800170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089900170209     C                   EVAL      InStringa =
090000180110     C                                %subst(wvindta:%len(%trim(wTag))+1)
090100180110     C* così ottengo una schiera di 2 elementi con 1) nr.progressivo 2) nr. colli+altro da splittare
090200180110     C                   EVAL      InSepara = '+'
090300170209     C                   EXSR      SR_SPLIT
090400180110     C                   EVAL      InStringa =
090500180110     C                                %trim(SkSplitCSV_5(2))
090600180110     C* così ottengo una schiera di 2 elementi con 1) nr.colli 2) tipologia colli
090700180110     C                   EVAL      InSepara = ':'
090800180110     C                   EXSR      SR_SPLIT
090900170209     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
091000170209     C                   EXSR      CHKNUM
091100170209     C                   IF        PiInt=*on
091200171218     C                   Z-ADD     PiVal         VABNCL
091300171218     C***                ADD       PiVal         VABNCL
091400170209     C                   ELSE
091500170209     C                   Z-ADD     1             wRecErr
091600180111     C                   SETON                                        31
091700180111     C                   EVAL      vinmsg = %trimr(vinmsg)
091800180111     C                             + ' ' + 'VABNCL'
091900170209     C                   ENDIF
092000170209     C                   ENDIF
092100171031     C*
092200171031     C***
092300180111     C* ...tipo record 'MEA+WT++KGM' (peso spedizione)
092400180111     C                   EVAL      wTag = 'MEA+WT++KGM'
092500171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092600171031     C                   EVAL      InStringa =
092700171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
092800171031     C                   EVAL      InSepara = ':'
092900171031     C                   EXSR      SR_SPLIT
093000171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
093100171031     C                   EXSR      CHKNUM
093200171031     C                   IF        PiNum=*on
093300171218     C                   Z-ADD     PiVal         VABPKB
093400171031     C                   ELSE
093500171031     C                   Z-ADD     1             wRecErr
093600180111     C                   SETON                                        31
093700180111     C                   EVAL      vinmsg = %trimr(vinmsg)
093800180111     C                             + ' ' + 'VABPKB'
093900171031     C                   ENDIF
094000171031     C                   ENDIF
094100171031     C*
094200171031     C***
094300171031     C* ...tipo record 'MEA+VOL++MTQ' (volume spedizione)
094400171108     C***                EVAL      wTag = 'MEA+VOL++MTQ'
094500171108     C                   EVAL      wTag = 'MEA+VOL+AAE+MTQ'
094600171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
094700171031     C                   EVAL      InStringa =
094800171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
094900171031     C                   EVAL      InSepara = ':'
095000171031     C                   EXSR      SR_SPLIT
095100171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
095200171031     C                   EXSR      CHKNUM
095300171031     C                   IF        PiNum=*on
095400171218     C                   Z-ADD     PiVal         VABVLB
095500171031     C                   ELSE
095600171031     C                   Z-ADD     1             wRecErr
095700180111     C                   SETON                                        31
095800180111     C                   EVAL      vinmsg = %trimr(vinmsg)
095900180111     C                             + ' ' + 'VABVLB'
096000171031     C                   ENDIF
096100171031     C                   ENDIF
096200170209     C*
096300170209     C***
096400180111     C* ...tipo record 'RFF+CU' (delivery adidas)
096500180111     C                   EVAL      wTag = 'RFF+CU'
096600171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
096700171218     C                   EVAL      wRFF += 1
096800171218     C                   EVAL      PtrRFF = %alloc(wRFF)
096900171218     C                   EVAL      SkRFF(wRFF) =
097000171218     C                               %subst(wvindta:%len(%trim(wTag))+2)
097100171218     C                   ENDIF
097200170209     C*
097300170209     C***
097400180111     C* ...tipo record 'RFF+CR' (riferimento spedizione)
097500180111     C***                EVAL      wTag = 'RFF+CR'
097600171031     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097700171031     C***                EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
097800171031     C***                EXSR      CHKNUM
097900171031     C***                IF        PiInt=*on
098000171031     C***                Z-ADD     PiVal         VABRMN
098100171031     C***                ELSE
098200171031     C***                EVAL      VABRMN = 1
098300171031     C***                Z-ADD     1             wRecErr
098400171031     C***                ENDIF
098500171031     C***                ENDIF
098600141028     C*
098700141028     C***
098800161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
098900171220     C* NOTA: questo traduttore tratta 2 casi:
099000171220     C* 1 = Sfusi: 1 sped. BRT (segm. UNH), 1 delivery adidas (segm. RFF), ed N coppie HU/segnacolli
099100171220     C*     (segm. GIN).
099200171220     C* 2 = Groupage: 1 sped. BRT (segm. UNH), N delivery adidas (segm. RFF), e 1 coppia
099300171220     C*     HU/segnacolli (segm. GIN).
099400171220     C* Nel segm. GIN ci sono massimo 5 coppie, il primo valore è BRT il secondo è adidas
099500171220     C* impostate così: GIN+BJ+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS'
099600171220     C* Quindi OGNI VOLTA che ho determinato un coppia, posso scrivere VAD10 perché:
099700171220     C* - nel caso 1 ho un solo RFF (che ho già recuperato) associato a tutte le coppie
099800171220     C* - nel caso 2 ho N RFF (che ho già recuperato) associati all'unica coppie
099900161013     C                   EVAL      wTag = 'GIN+BJ'
100000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100100150526     C                   EVAL      InStringa =
100200150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
100300171220     C                   EVAL      InSepara = '+'
100400171220     C                   EXSR      SR_SPLIT
100500171220     C                   EVAL      SkSplitCSV_Sav = SkSplitCSV_5
100600171220     C                   FOR       wI = 1 to 5
100700171220     C                   IF        %trim(SkSplitCSV_Sav(wI)) <> *blanks
100800171220     C                   EVAL      InStringa =
100900180111     C                             %trim(SkSplitCSV_Sav(wI))
101000171220     C                   EVAL      InSepara = ':'
101100171220     C                   EXSR      SR_SPLIT
101200180115     C                   EVAL      H_VADNCD = %dec(%trim(SkSplitCSV_5(1)):7:0)
101300180115     C                   EVAL      VADNCD = H_VADNCD
101400180115     C                   EVAL      H_VADUN1 = %trim(SkSplitCSV_5(2))
101500171220     C                   EXSR      WRIVAD1
101600171220     C                   ELSE
101700171220     C                   LEAVE
101800150526     C                   ENDIF
101900171220     C                   ENDFOR
102000171108     C                   ENDIF
102100141028     C*
102200141028     C***
102300141028     C* ...tipo record '???' (PDF per packing-list)
102400141028     C                   EVAL      wTag = '???PDF'
102500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
102600141028     C*
102700141028     C* ......VATNOT_P
102800141028     C                   if        *in53
102900141028     C*
103000141028     C* Solo se valorizzato RMN
103100141028     C                   if        VABRMA <> *blanks
103200141028     C                   eval      pIn_MaskPDF =
103300141028     C                                     %trim(%subst(wvindta:1292:100))
103400141028     C                   call      'TIS7P2SER'
103500141028     C                   parm      'W'           pIn_Opz           1
103600141028     C                   parm                    tivlrds
103700141028     C                   parm                    EDIVABDS
103800141028     C                   parm      '10'          pIn_CdIdAz        2
103900141028     C                   parm                    pIn_MaskPDF      50
104000141028     C                   parm      *blanks       pOut_Esito        1
104100141028     C                   endif
104200141028     C                   endif
104300141028     C*
104400141028     C                   ENDIF
104500141028     C*
104600141028     C***
104700141028     C* ...tipo record 'UNT' (fine bolla)
104800141028     C                   EVAL      wTag = 'UNT'
104900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
105000141028     C  N31              ADD       1             §CTROKVB
105100141028     C  N31              exsr      CHKWRI
105200140117     C  N31              WRITE     EDIVAB00
105300150526     C                   ENDIF
105400010202     C*
105500000801     C* Ebbene...
105600000801     C                   ADD       1             §CTRMO
105700141028     C                   IF        wRecErr <> *zeros
105800000801     C                   ADD       1             §CTRNO
105900000801     C                   EVAL      vinflg = '2'
106000000801     C                   ENDIF
106100000801     C*
106200000801     C                   ENDSR
106300010202     C*----------------------------------------------------*
106400171219     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
106500010202     C*----------------------------------------------------*
106600171219     C     PREVAD1T      BEGSR
106700010202     C*
106800171219     C* Compongo il nome del membro da dare al EDIVATWR
106900010202     C                   eval      parmbr = vlrhdl
107000010202     C                   movel     'M'           parmbr
107100060113     C                   eval      parccm = vlrksc
107200010202     C                   eval      paropz = '1'
107300010202     C* Effettuo la chiamata al CLLE preposto
107400180115     C***                call(e)   'TITVEVD1TC'
107500180115     C                   call(e)   'TITVEVDTC'
107600010202     C                   parm                    parccm
107700010202     C                   parm                    parmbr
107800010202     C                   parm                    paropz
107900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
108000010202     C                   if        %error
108100010202     C                   movel     '1'           chkcall
108200010202     C                   else
108300010202     C                   movel     '0'           chkcall
108400010202     C                   endif
108500010202     C*
108600010202     C                   ENDSR
108700141028     C*----------------------------------------------------*
108800141028     C*  CONTROLLO NUMERICITA' CAMPI
108900141028     C*----------------------------------------------------*
109000141028     C     CHKNUM        BEGSR
109100141028     C*
109200141028     C                   IF        PiDecChr = *blanks
109300141028     C                   EVAL      PiDecChr = CharNUM
109400141028     C                   ENDIF
109500141028     C*
109600141028     C                   callp     UBISNUM_Check(PiStr
109700141028     C                                          :PiDecChr
109800141028     C                                          :PiVal
109900141028     C                                          :PiNum
110000141028     C                                          :PiInt)
110100141028     C*
110200141028     C                   ENDSR
110300141028     C***
110400141028
110500141028
110600141028     C*----------------------------------------------------*
110700141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
110800141028     C*----------------------------------------------------*
110900141028     C     SR_SPLIT      BEGSR
111000141028     C*
111100141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
111200141028     C                   CALL      'XSPLIT2'
111300141028     C                   PARM                    InStringa
111400141028     C                   PARM                    InSepara
111500141028     C                   PARM      '5'           InTypeOut
111600141028     C                   PARM      ''            wSkParam
111700141028     C                   PARM                    OutErrore
111800141028     C                   PARM                    OutMsg
111900141028     C                   MOVEA     wSkParam      SkSplitCSV_5
112000141028     C*
112100141028     C                   ENDSR
112200141028
112300141028
112400141028     C*----------------------------------------------------*
112500171220     C*  SCRITTURA BUFFER EDIVAD10F
112600141028     C*----------------------------------------------------*
112700171220     C     WRIVAD1       BEGSR
112800180115     C*
112900180115     C                   EVAL      VADFGS = VABFGS
113000180115     C                   EVAL      VADCCM = VABCCM
113100180115     C                   EVAL      VADLNP = VABLNP
113200180115     C                   EVAL      VADAAS = VABAAS
113300180115     C                   EVAL      VADNRS = VABNRS
113400180115     C                   EVAL      VADNSP = VABNSP
113500180115     C                   EVAL      VADCMR = VABCMR
113600180115     C                   EVAL      VADCNT = VABCNT
113700180115     C                   EVAL      VADNCL = VABNCL
113800180115     C                   WRITE     EDIVAD00
113900141028     C*
114000180115     C                   IF        H_VADUN1 <> *blanks
114100171220     C                   EVAL      wJ = 0
114200171220     C* devo scrivere tanti rcd quante sono le delivery adidas memorizzate nella schiera SkRFF
114300171220     C* che però è a dimensione variabile
114400171220     C                   DO        *hival
114500171220     C                   EVAL      wJ += 1
114600171220     C                   IF        wJ > %elem(SkRFF)
114700171220     C                   LEAVE
114800171220     C                   ENDIF
114900180115     C*
115000180115     C                   EVAL      H_VADFGS = VABFGS
115100180115     C                   EVAL      H_VADCCM = VABCCM
115200180115     C                   EVAL      H_VADLNP = VABLNP
115300180115     C                   EVAL      H_VADAAS = VABAAS
115400180115     C                   EVAL      H_VADNRS = VABNRS
115500180115     C                   EVAL      H_VADNSP = VABNSP
115600180115     C                   EVAL      H_VADCMR = VABCMR
115700180115     C                   EVAL      H_VADCNT = VABCNT
115800180115     C                   EVAL      H_VADNCL = VABNCL
115900180115     C                   EVAL      H_VADUN2 = SkRFF(wJ)
116000171219     C                   WRITE     EDIVAD100
116100171220     C                   ENDDO
116200171220     C                   ENDIF
116300141028     C*
116400141028     C                   ENDSR
116500000801
116600171219
116700171219     C*----------------------------------------------------*
116800171219     C*  SCRITTURA BUFFER EDIVAT
116900171219     C*----------------------------------------------------*
117000171219     C     WRIVAT        BEGSR
117100171219     C*
117200171219     C                   IF        VATNOT <> *blanks
117300171219     C                   EVAL      VATFGS = VABFGS
117400171219     C                   EVAL      VATCCM = VABCCM
117500171219     C                   EVAL      VATLNP = VABLNP
117600171219     C                   EVAL      VATAAS = VABAAS
117700171219     C                   EVAL      VATNRS = VABNRS
117800171219     C                   EVAL      VATNSP = VABNSP
117900171219     C                   EVAL      VATCMR = VABCMR
118000171219     C                   EVAL      VATCNT = VABCNT
118100171219     C                   ADD       1             §CTROKVT
118200171219     C                   WRITE     EDIVAT00
118300171219     C                   ENDIF
118400171219     C*
118500171219     C                   ENDSR
118600171219
118700011113
118800011113     C*----------------------------------------------------*
118900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
119000011113     C*----------------------------------------------------*
119100011113     C     CHKIMPDIV     BEGSR
119200011113     C*
119300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
119400011113     C                   Z-ADD     *zeros        wrkDec            9 9
119500011113     C*
119600011113     C* Come prima cosa effettuo considerazioni sulla divisa
119700011113     C                   IF        vabIAS > *zeros
119800011113     C                   IF        vabVAS <> 'EUR'
119900011113     C                   EVAL      vabVAS =  'ITL'
120000011113     C                   ENDIF
120100011113     C                   ENDIF
120200011113     C*
120300011113     C                   IF        vabCAS > *zeros
120400011113     C                   IF        vabVCA <> 'EUR'
120500011113     C                   EVAL      vabVCA =  'ITL'
120600011113     C                   ENDIF
120700011113     C                   ENDIF
120800011113     C*
120900011113     C                   IF        vabVMD > *zeros
121000020305     C                   IF        vabVAD <> 'EUR'
121100011113     C                   EVAL      vabVAD =  'ITL'
121200011113     C                   ENDIF
121300011113     C                   ENDIF
121400011113     C*
121500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
121600011113     C                   Z-ADD     vabIAS        wrkDec
121700011113     C                   IF        wrkDec > *zeros
121800011113     C                   IF        vabVAS = 'ITL'
121900011113     C                   EVAL      vabIAS = *zeros
122000011113     C                   ENDIF
122100011113     C                   ENDIF
122200011113     C*
122300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
122400011113     C                   Z-ADD     vabCAS        wrkDec
122500011113     C                   IF        wrkDec > *zeros
122600011113     C                   IF        vabVCA = 'ITL'
122700011113     C                   EVAL      vabCAS = *zeros
122800011113     C                   ENDIF
122900011113     C                   ENDIF
123000011113     C*
123100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
123200011113     C                   Z-ADD     vabVMD        wrkDec
123300011113     C                   IF        wrkDec > *zeros
123400011113     C                   IF        vabVAD = 'ITL'
123500011113     C                   EVAL      vabVMD = *zeros
123600011113     C                   ENDIF
123700011113     C                   ENDIF
123800011113     C*
123900011113     C                   ENDSR
124000011113     C***
124100011113
124200011113
124300000801
124400000801
124500990920      /TITLE Invio dei dati al punto operativo.
124600010202     C     invio         BEGSR
124700990920     C*
124800171218     C* 1° invio EDIVAD
124900010201     C                   reset                   dscmz
125000010201     C                   move      vlrpoi        cmzdst
125100180115     C                   eval      cmzfld = 'EDIVADWR'
125200010201     C                   eval      cmzmbd = vlrhdl
125300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
125400021009     C***                if        prmfir = *blanks
125500180115     C                   eval      cmzfla = 'EDIVAD0F'
125600180115     C                   eval      cmzmba = 'EDIVAD0F'
125700021009     C***                else
125800021009     C***                eval      cmzfla = prmfir
125900021009     C***                eval      cmzmba = prmfir
126000021009     C***                endif
126100010201     C                   eval      cmznrr = *zeros
126200020305     C                   move      §ctrokvt      cmznrr
126300021018     C                   eval      cmzlba = vlrfl1
126400010201     C                   call(e)   'TIS711C'
126500010201     C                   parm                    dscmz
126600010201     C                   parm      *blanks       esito
126700010205     C                   if        %error
126800010205     C                             or cmzerr = '1'
126900010205     C                             or esito  = '1'
127000010205     C                   eval      wrkesito = '3'
127100010205     C                   else
127200171219     C*
127300171219     C* 2° invio EDIVAT
127400171219     C                   reset                   dscmz
127500171219     C                   move      vlrpoi        cmzdst
127600171219     C                   eval      cmzfld = 'EDIVATWR'
127700171219     C                   eval      cmzmbd = vlrhdl
127800171219     C                   eval      %subst(cmzmbd:1:1) = 'M'
127900171219     C***                if        prmfir = *blanks
128000171219     C                   eval      cmzfla = 'EDIVAT0F'
128100171219     C                   eval      cmzmba = 'EDIVAT0F'
128200171219     C***                else
128300171219     C***                eval      cmzfla = prmfir
128400171219     C***                eval      cmzmba = prmfir
128500171219     C***                endif
128600171219     C                   eval      cmznrr = *zeros
128700171219     C                   move      §ctrokvt      cmznrr
128800171219     C                   eval      cmzlba = vlrfl1
128900171219     C                   call(e)   'TIS711C'
129000171219     C                   parm                    dscmz
129100171219     C                   parm      *blanks       esito
129200171219     C                   if        %error
129300171219     C                             or cmzerr = '1'
129400171219     C                             or esito  = '1'
129500171219     C                   eval      wrkesito = '3'
129600171219     C                   else
129700010201     C*
129800171219     C* 3° invio EDIVAB
129900010201     C                   reset                   dscmz
130000010201     C                   move      vlrpoi        cmzdst
130100010201     C                   eval      cmzfld = vlrfou
130200010201     C                   eval      cmzmbd = vlrhdl
130300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
130400021009     C***                if        prmfir = *blanks
130500140117     C                   eval      cmzfla = 'EDIVAB0F'
130600140117     C                   eval      cmzmba = 'EDIVAB0F'
130700021009     C***                else
130800021009     C***                eval      cmzfla = prmfir
130900021009     C***                eval      cmzmba = prmfir
131000021009     C***                endif
131100010201     C                   eval      cmznrr = *zeros
131200010201     C                   move      §ctrokvb      cmznrr
131300021018     C                   eval      cmzlba = vlrfl1
131400010201     C                   call(e)   'TIS711C'
131500010201     C                   parm                    dscmz
131600010201     C                   parm      *blanks       esito
131700010201     C                   if        %error
131800010201     C                             or cmzerr = '1'
131900010201     C                             or esito  = '1'
132000010201     C                   eval      wrkesito = '3'
132100010201     C                   endif
132200010205     C                   endif
132300171219     C                   endif
132400990920     C*
132500000613     C                   ENDSR
132600000613     C***
132700070411
132800141028
132900070411     C     *pssr         BEGSR
133000070411     C*
133100070411     C                   if        %open(tivin00r)
133200070411     C                   close     tivin00r
133300070411     C                   endif
133400140117     C                   if        %open(edivabwr)
133500140117     C                   close     edivabwr
133600070411     C                   endif
133700171219     C                   if        %open(EDIVAD1wr)
133800171219     C                   close     EDIVAD1wr
133900070411     C                   endif
134000180115     C                   if        %open(EDIVADwr)
134100180115     C                   close     EDIVADwr
134200171219     C                   endif
134300180115     C                   if        %open(EDIVATwr)
134400180115     C                   close     EDIVATwr
134500180115     C                   endif
134600070411     C*
134700070411     C* Effettuo la chiamata al CLLE preposto
134800180115     C***                call(e)   'TITVEVD1TC'
134900180115     C                   call(e)   'TITVEVDTC'
135000070411     C                   parm                    parccm
135100070411     C                   parm                    parmbr
135200070411     C                   parm      '2'           paropz
135300070411     C*
135400070411     C                   eval      wrkesito = '2'
135500070411     C*
135600070411     C                   seton                                        LR
135700070411     C*
135800070411     C                   ENDSR     '*CANCL'
135900070411     C***
136000161018
136100161018
136200161018     C*--------------------------------------------------------
136300161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
136400161018     C*--------------------------------------------------------
136500161018     C     CARTAB        BEGSR
136600161018     C*
136700161018     C* TABELLA '15' - NAZIONI
136800161018     C                   clear                   skNAZISO
136900161018     C                   clear                   skNAZBAR
137000161018     C                   eval      tblKUT = 1
137100161018     C                   eval      tblCOD = '15'
137200161018     C     KEYtabP       setll     tabel00f
137300161018     C     KEYtabP       reade     tabel00f
137400161018     C                   dow       not %eof(tabel00f)
137500161018     C                   if        tblFLG = *blanks
137600161018     C                   movel(p)  tblUNI        ds15
137700161018     C                   add       1             jNAZ
137800161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
137900161018     C                   eval      skNAZISO(jNAZ) = §15COD
138000161018     C                   endif
138100161018     C     KEYtabP       reade     tabel00f
138200161018     C                   enddo
138300161018     C*
138400161018     C                   ENDSR
138500161018     C***
138600070411
138700990910
138800000613     C     *inzsr        BEGSR
138900990910     C*
139000990910     C     *entry        plist
139100990920     C                   parm                    tivlrds
139200990921     C                   parm      wrkesito      esito
139300000724     C                   parm                    prmlit
139400000710     C                   parm                    prmfir
139500000613     C*
139600000830     C* CALCOLA LA DATA CORRENTE
139700140117     C                   time                    wn14             14 0
139800140117     C                   movel     wn14          oracor            6 0          *ORA
139900100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
140000100722     C                   eval      datcor = %dec(%date() : *ISO)
140100161018     C*
140200161018     C* Chiave su TABEL00F - parziale
140300161018     C     KEYtabP       KLIST
140400161018     C                   KFLD                    tblKUT
140500161018     C                   KFLD                    tblCOD
140600000830     C*
140700000613     C                   ENDSR
140800000613     C***
