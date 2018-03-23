000100160610      /TITLE Upload via Internet: traduzione in FNVAOWWR
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400160613     Ftntbe01l  iF   E           K DISK
000500990910     Ftivin00r  uF   E             DISK    usropn
000600160610     FFNVAOwwr  O    E             DISK    usropn
000700160613     Ftiori00f  O    E             DISK
000800160613     FTIVGD00F  O    E             DISK
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990920     D dscmz         e ds                  inz
001400160613     D psds           sds
001500160613     D  procname         *PROC
001600160613     D tivlrds       e ds                  extname(tivlr00f)
001700070719     D tisi95ds      e ds
001800170919     D fnvapds       e ds                  extname(fnvap00f)
001900170919     D dorm01        e ds
002000170919     D dfar          e ds
002100160613     D trul33ds      e ds
002200160613     D kpjba         e ds
002300160613     D esito           s              1
002400160613     D prmlit          s             10
002500160613     D prmfir          s             10
002600160613     D wrkesito        s                   like(esito)
002700160613     D wTag            s            128    VARYING INZ
002800160613     D wRecErr         s              1  0 INZ(*zeros)
002900160613     D wVINDTA         s                   LIKE(vinDTA) INZ
003000160613     D Cli_Abil        s              1    inz(*off)
003100160613     D woriIDC         s                   like(oriIDC) inz(*blanks)
003200160613     D §numori         s              7  0 inz
003300160606
003400141028
003500141028     D*------------------
003600141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003700141028     D*------------------
003800141028     D CharNUM         S              1
003900141028     D CharSOS         S              1
004000141028     D posDaDft        S              3  0 INZ(*zeros)
004100141028     D posADft         S              3  0 INZ(*zeros)
004200141028     D wGiroDft        s              1  0 INZ(*zeros)
004300141028
004400141028
004500141028     D*------------------
004600141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
004700141028     D*------------------
004800141028     D InStringa       S          65535A   VARYING                              (stringa input)
004900141028     D InSepara        S             10A                                        (separatore)
005000141028     D InTypeOut       S              1A                                        (tipo output)
005100141028     D wSkParam        S          65535A                                        (output)
005200141028     D OutErrore       S              1A                                        (flag errore)
005300141028     D OutMsg          S             80A                                        (messaggio errore)
005400141028     D SkSplitCSV_5    S            256    DIM(256)
005500990908
005600141028
005700141028     D*------------------
005800141028     D* LINKING A DEFINIZIONI ESTERNE
005900141028     D*------------------
006000141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006100141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006200160613
006300160613     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006400160613     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006500141028
006600010201
006700010201
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000613     C*
007100160613     C                   EXSR      RWFILE
007200070719     C*
007300070719     C* Esegue lancio TISI95R solo x chiusura
007400070719     C                   CLEAR                   TISI95DS
007500070719     C                   EVAL      I95TLA = 'C'
007600070719     C                   CALL      'TISI95R'
007700070719     C                   PARM                    TISI95DS
007800000616     C*
007900010201     C                   seton                                        LR
008000990908
008100000801
008200910830     C*--------------------------------------------------------
008300160610     C* RWFILE   LEGGE tivin00r E SCRIVE FNVAOWWR
008400910830     C*--------------------------------------------------------
008500040526     C     RWFILE        BEGSR
008600990910     C*
008700990914     C                   if        not %open(tivin00r)
008800990908     C                   open      tivin00r
008900990914     C                   endif
009000160613     C                   if        not %open(fnvaowwr)
009100160613     C                   open      fnvaowwr
009200990914     C                   endif
009300990910     C*
009400160613     C                   clear                   §CTROK            5 0
009500000801     C                   clear                   §CTRMO            5 0
009600000801     C                   clear                   §CTRNO            5 0
009700141028     C*
009800141028     C* Inizializzazioni fuori ciclo
009900160613     C                   SETOFF                                       40
010000141028     C                   EXSR      INZVAR
010100150127     C*
010200921023     C                   DO        *HIVAL
010300990913     C*
010400990915     C                   READ      tivin00r                               70
010500040910     C                   if        vindta > *blanks
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400141028     C*
011500141028     C* "normalizzo" il dato di input
011600141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
011700141028     C                                             %len(%trim(vindta))-1))
011800160209     C*
011900160209     C* Elimino le "duple di controllo EDIFACT"
012000160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
012100160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
012200160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
012300160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
012400160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
012500040910     C*
012600040910     C* Eseguo routine d traduzione
012700160613     C                   exsr      impvao
012800040802     C*
012900010305     C                   endif
013000000905     C*
013100000905     C                   else
013200000905     C                   eval      vinflg = '1'
013300000905     C                   endif
013400000905     C*
013500000905     C  N70              update    tivin000
013600000905     C*
013700991022     C  N70              ENDdo
013800160613     C*
013900990910     C* Se non ci sono record con errori ...
014000000710     C                   if        §ctrno = 0
014100990910     C* ... restituisco esito OK.
014200990921     C                   eval      wrkesito = '0'
014300990910     C                   else
014400160613     C                   if        §ctrok > 0
014500990921     C                   eval      wrkesito = '1'
014600000710     C                   else
014700000710     C                   eval      wrkesito = '2'
014800990910     C                   endif
014900000710     C                   endif
015000990910     C*
015100990914     C                   if        %open(tivin00r)
015200990908     C                   close     tivin00r
015300990914     C                   endif
015400160613     C                   if        %open(fnvaowwr)
015500160613     C                   close     fnvaowwr
015600990914     C                   endif
015700990910     C*
015800160613     C                   if        §ctrok > 0
015900000724     C                             and vlrpoi <> *zeros
016000010202     C                   exsr      invio
016100990920     C                   endif
016200990920     C*
016300910830     C                   ENDSR
016400000613     C***
016500140922
016600160613
016700140922     C*----------------------------------------------------*
016800140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
016900140922     C*----------------------------------------------------*
017000140922     C     CHKWRI        BEGSR
017100160613     C*
017200160613     C* ......VAOPOE (TEST /PROD)
017300160613     C                   if        *in55
017400160613     C                   eval      VAOPOE = 789
017500160613     C                   endif
017600160613     C*
017700160613     C* ......VAOPAG
017800160613     C                   if        VAOPAG = *blanks
017900160613     C                   eval      VAOPAG = 'O'
018000160613     C                   endif
018100160613     C*
018200160613     C* ......VAODAR
018300160613     C                   eval      VAODAR = savDAR
018400160613     C*
018500160613     C* ......VAOORR
018600160613     C                   eval      VAOORR = savORR
018700160613     C                   if        VAOORR = *zeros
018800160613     C                   eval      VAOORR = 0800
018900160613     C                   endif
019000160613     C*
019100160613     C* ......VAONCL
019200160613     C                   if        VAONCL = *zeros
019300160613     C                   eval      VAONCL = cntVAONCL
019400150127     C                   endif
019500150127     C*
019600160613     C* ......VAOPRO
019700160613     C                   if        VAOPRO = *blanks
019800160613     C* Reperisco la provincia dal CAP e dalla località
019900160613     C                   IF        VAOLOO <> *blanks AND
020000160613     C                             VAOCAO <> *blanks AND
020100160613     C                             VAONAO  = *blanks
020200141028     C                   CLEAR                   TISI95DS
020300141028     C                   EVAL      I95TCN = '3'
020400141028     C                   Z-ADD     datcor        I95DAT
020500160613     C                   EVAL      I95CAP = VAOCAO
020600160613     C                   EVAL      I95LOC = VAOLOO
020700141028     C                   CALL      'TISI95R'
020800141028     C                   PARM                    TISI95DS
020900160613     C                   EVAL      VAOPRO = O95PRV
021000141028     C                   ENDIF
021100141028     C                   endif
021200160613     C*
021300160613     C* ......VAOPRC
021400160613     C                   if        VAOPRC = *blanks
021500160613     C* Reperisco la provincia dal CAP e dalla località
021600160613     C                   IF        VAOLOC <> *blanks AND
021700160613     C                             VAOCAC <> *blanks AND
021800160613     C                             VAONAC  = *blanks
021900160613     C                   CLEAR                   TISI95DS
022000160613     C                   EVAL      I95TCN = '3'
022100160613     C                   Z-ADD     datcor        I95DAT
022200160613     C                   EVAL      I95CAP = VAOCAC
022300160613     C                   EVAL      I95LOC = VAOLOC
022400160613     C                   CALL      'TISI95R'
022500160613     C                   PARM                    TISI95DS
022600160613     C                   EVAL      VAOPRC = O95PRV
022700160613     C                   ENDIF
022800160613     C                   endif
022900160613     C*
023000160613     C* ......VAOPRR
023100160613     C                   if        VAOPRR = *blanks
023200160613     C* Reperisco la provincia dal CAP e dalla località
023300160613     C                   IF        VAOLOR <> *blanks AND
023400160613     C                             VAOCAR <> *blanks AND
023500160613     C                             VAONAR  = *blanks
023600160613     C                   CLEAR                   TISI95DS
023700160613     C                   EVAL      I95TCN = '3'
023800160613     C                   Z-ADD     datcor        I95DAT
023900160613     C                   EVAL      I95CAP = VAOCAR
024000160613     C                   EVAL      I95LOC = VAOLOR
024100160613     C                   CALL      'TISI95R'
024200160613     C                   PARM                    TISI95DS
024300160613     C                   EVAL      VAOPRR = O95PRV
024400160613     C                   ENDIF
024500160613     C                   endif
024600140922     C*
024700140922     C                   ENDSR
024800160615
024900160615
025000160615     C*----------------------------------------------------*
025100160615     C*  APPLICAZIONI PERSONALIZZAZIONI
025200160615     C*----------------------------------------------------*
025300170919     C     APLYPERS      BEGSR
025400160615     C*
025500170919     C* ......VAOxxx
025600160615     C*
025700160615     C                   ENDSR
025800160615
025900160615
026000141028     C*----------------------------------------------------*
026100141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026200141028     C*----------------------------------------------------*
026300141028     C     INZVAR        BEGSR
026400141028     C*
026500141028     C* Inizializzo variabili di wrk
026600141028     C                   Z-ADD     *zeros        Num5_0            5 0
026700160613     C                   Z-ADD     *zeros        cntVAONCL         5 0
026800141028     C                   CLEAR                   SkSplitCSV_5
026900141028     C*
027000141028     C* Reimposto i valori di default
027100141028     C                   EXSR      DEFCAM
027200141028     C*
027300141028     C                   ENDSR
027400141028     C*----------------------------------------------------*
027500141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
027600141028     C*----------------------------------------------------*
027700141028     C     DEFCAM        BEGSR
027800141028     C*
027900160613     C                   CLEAR                   FNVAO000
028000160613     C                   CLEAR                   TIORI000
028100141028     C*
028200160613     C* Reperisco se elaborazione in TEST (*IN55) o PROD (not *IN55)
028300160613     C                   SETOFF                                       55
028400160613     C                   IF        %subst(vlrppt:1:1)='T'
028500160613     C                   SETON                                        55
028600160613     C                   ENDIF
028700160613     C*
028800160613     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
028900160613     C* e delimitatore testo.
029000160613     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029100141028     C*
029200141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
029300141028     C                   EVAL      CharSOS = CharNUM
029400160613     C*
029500160613     C* Imposto i valori di default
029600160613     C                   CLEAR                   DORM01
029700160613     C                   EVAL      VAOFLO = DORM01
029800160613     C                   EVAL      VAOTCO = 'F'
029900160613     C                   EVAL      VAOTOR = 'S'
030000160615     C                   EVAL      VAOPAG = 'O'
030100160615     C                   EVAL      VAOPOE = vlrpoi
030200160615     C*
030300160613     C* IMPOSTO I CAMPI PERSONALIZZATI
030400160613     C*
030500160613     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030600160613     C                   EVAL      posDaDft = 1
030700160613     C                   EVAL      posADft  = 0
030800160613     C                   EVAL      wGiroDft = 0
030900160613     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031000160613     C                             posDaDft > 0
031100160613     C*
031200160613     C* Gestisco il 1° giro
031300160613     C                   IF        wGiroDft = 0
031400160613     C* Eseguo lo scan x trovare l'inizio del campo corrente
031500160613     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031600160613     C* Incremento il contatore dei "giri"
031700160613     C                   EVAL      wGiroDft = 1
031800160613     C                   ELSE
031900160613     C                   EVAL      posDaDft = posADft
032000160613     C                   ENDIF
032100160613     C* Eseguo lo scan x trovare la fine del campo corrente
032200160613     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032300160613     C*
032400160613     C* A questo "estraggo" il parametro (campo e valore) corrente...
032500160613     C* ...solo se entrambe le posizini (DA/A) sono > 0
032600160613     C                   IF        posDaDft > 0 AND
032700160613     C                             posADft  > 0
032800160613     C* §ONC (ORM Non Commissionato)
032900160613     C                   IF        %subst(
033000160613     C                             %subst(vlrppt:posDaDft+1:
033100160613     C                             posADft-posDaDft-1):1:4)
033200160613     C                             = '§ONC'
033300160613     C                   MOVEL     *blanks       sav§ONC           1
033400160613     C                   EVAL      sav§ONC=%trim(%subst(
033500160613     C                             %subst(vlrppt:posDaDft+1:
033600160613     C                             posADft-posDaDft-1):5:posADft-posDaDft-1-4))
033700160613     C                   ENDIF
033800160613     C* POE
033900160613     C                   IF        %subst(
034000160613     C                             %subst(vlrppt:posDaDft+1:
034100160613     C                             posADft-posDaDft-1):1:3)
034200160613     C                             = 'POE'
034300160613     C                   EVAL      PiStr=%trim(%subst(
034400160613     C                             %subst(vlrppt:posDaDft+1:
034500160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034600160613     C                   EXSR      CHKNUM
034700160613     C                   IF        PiInt=*on
034800160613     C                   Z-ADD     PiVal         VAOPOE
034900160613     C                   ENDIF
035000160613     C                   ENDIF
035100160613     C* TOR
035200160613     C                   IF        %subst(
035300160613     C                             %subst(vlrppt:posDaDft+1:
035400160613     C                             posADft-posDaDft-1):1:3)
035500160613     C                             = 'TOR'
035600160613     C                   EVAL      VAOTOR=%trim(%subst(
035700160613     C                             %subst(vlrppt:posDaDft+1:
035800160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035900160613     C                   ENDIF
036000160613     C* TCO
036100160613     C                   IF        %subst(
036200160613     C                             %subst(vlrppt:posDaDft+1:
036300160613     C                             posADft-posDaDft-1):1:3)
036400160613     C                             = 'TCO'
036500160613     C                   EVAL      VAOTCO=%trim(%subst(
036600160613     C                             %subst(vlrppt:posDaDft+1:
036700160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036800160613     C                   ENDIF
036900160613     C* COR
037000160613     C                   IF        %subst(
037100160613     C                             %subst(vlrppt:posDaDft+1:
037200160613     C                             posADft-posDaDft-1):1:3)
037300160613     C                             = 'COR'
037400160613     C                   EVAL      PiStr=%trim(%subst(
037500160613     C                             %subst(vlrppt:posDaDft+1:
037600160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037700160613     C                   EXSR      CHKNUM
037800160613     C                   IF        PiInt=*on
037900160613     C                   Z-ADD     PiVal         VAOCOR
038000160613     C                   ENDIF
038100160613     C                   ENDIF
038200160613     C* CRC
038300160613     C                   IF        %subst(
038400160613     C                             %subst(vlrppt:posDaDft+1:
038500160613     C                             posADft-posDaDft-1):1:3)
038600160613     C                             = 'CRC'
038700160613     C                   EVAL      PiStr=%trim(%subst(
038800160613     C                             %subst(vlrppt:posDaDft+1:
038900160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039000160613     C                   EXSR      CHKNUM
039100160613     C                   IF        PiInt=*on
039200160613     C                   Z-ADD     PiVal         VAOCRC
039300160613     C                   ENDIF
039400160613     C                   ENDIF
039500160613     C* FFD
039600160613     C                   IF        %subst(
039700160613     C                             %subst(vlrppt:posDaDft+1:
039800160613     C                             posADft-posDaDft-1):1:3)
039900160613     C                             = 'FFD'
040000160613     C                   EVAL      VAOFFD=%trim(%subst(
040100160613     C                             %subst(vlrppt:posDaDft+1:
040200160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040300160613     C                   ENDIF
040400160613     C* ORR
040500160613     C                   IF        %subst(
040600160613     C                             %subst(vlrppt:posDaDft+1:
040700160613     C                             posADft-posDaDft-1):1:3)
040800160613     C                             = 'ORR'
040900160613     C                   EVAL      PiStr=%trim(%subst(
041000160613     C                             %subst(vlrppt:posDaDft+1:
041100160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200160613     C                   EXSR      CHKNUM
041300160613     C                   IF        PiInt=*on
041400160613     C                   Z-ADD     PiVal         VAOORR
041500160613     C                   ENDIF
041600160613     C                   ENDIF
041700160613     C* NCL
041800160613     C                   IF        %subst(
041900160613     C                             %subst(vlrppt:posDaDft+1:
042000160613     C                             posADft-posDaDft-1):1:3)
042100160613     C                             = 'NCL'
042200160613     C                   EVAL      PiStr=%trim(%subst(
042300160613     C                             %subst(vlrppt:posDaDft+1:
042400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500160613     C                   EXSR      CHKNUM
042600160613     C                   IF        PiInt=*on
042700160613     C                   Z-ADD     PiVal         VAONCL
042800160613     C                   ENDIF
042900160613     C                   ENDIF
043000160613     C* PKG
043100160613     C                   IF        %subst(
043200160613     C                             %subst(vlrppt:posDaDft+1:
043300160613     C                             posADft-posDaDft-1):1:3)
043400160613     C                             = 'PKG'
043500160613     C                   EVAL      PiStr=%trim(%subst(
043600160613     C                             %subst(vlrppt:posDaDft+1:
043700160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800160613     C                   EXSR      CHKNUM
043900160613     C                   IF        PiNum=*on
044000160613     C                   Z-ADD     PiVal         VAOPKG
044100160613     C                   ENDIF
044200160613     C                   ENDIF
044300160613     C* VLM
044400160613     C                   IF        %subst(
044500160613     C                             %subst(vlrppt:posDaDft+1:
044600160613     C                             posADft-posDaDft-1):1:3)
044700160613     C                             = 'VLM'
044800160613     C                   EVAL      PiStr=%trim(%subst(
044900160613     C                             %subst(vlrppt:posDaDft+1:
045000160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100160613     C                   EXSR      CHKNUM
045200160613     C                   IF        PiNum=*on
045300160613     C                   Z-ADD     PiVal         VAOVLM
045400160613     C                   ENDIF
045500160613     C                   ENDIF
045600160613     C* NAM
045700160613     C                   IF        %subst(
045800160613     C                             %subst(vlrppt:posDaDft+1:
045900160613     C                             posADft-posDaDft-1):1:3)
046000160613     C                             = 'NAM'
046100160613     C                   EVAL      VAONAM=%trim(%subst(
046200160613     C                             %subst(vlrppt:posDaDft+1:
046300160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400160613     C                   ENDIF
046500160613     C* PAG
046600160613     C                   IF        %subst(
046700160613     C                             %subst(vlrppt:posDaDft+1:
046800160613     C                             posADft-posDaDft-1):1:3)
046900160613     C                             = 'PAG'
047000160613     C                   EVAL      VAOPAG=%trim(%subst(
047100160613     C                             %subst(vlrppt:posDaDft+1:
047200160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300160613     C                   ENDIF
047400160613     C* KSC
047500160613     C                   IF        %subst(
047600160613     C                             %subst(vlrppt:posDaDft+1:
047700160613     C                             posADft-posDaDft-1):1:3)
047800160613     C                             = 'KSC'
047900160613     C                   EVAL      PiStr=%trim(%subst(
048000160613     C                             %subst(vlrppt:posDaDft+1:
048100160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048200160613     C                   EXSR      CHKNUM
048300160613     C                   IF        PiInt=*on
048400160613     C                   Z-ADD     PiVal         VAOKSC
048500160613     C                   ENDIF
048600160613     C                   ENDIF
048700160613     C* CTR
048800160613     C                   IF        %subst(
048900160613     C                             %subst(vlrppt:posDaDft+1:
049000160613     C                             posADft-posDaDft-1):1:3)
049100160613     C                             = 'CTR'
049200160613     C                   EVAL      VAOCTR=%trim(%subst(
049300160613     C                             %subst(vlrppt:posDaDft+1:
049400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049500160613     C                   ENDIF
049600160613     C* POC
049700160613     C                   IF        %subst(
049800160613     C                             %subst(vlrppt:posDaDft+1:
049900160613     C                             posADft-posDaDft-1):1:3)
050000160613     C                             = 'POC'
050100160613     C                   EVAL      PiStr=%trim(%subst(
050200160613     C                             %subst(vlrppt:posDaDft+1:
050300160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050400160613     C                   EXSR      CHKNUM
050500160613     C                   IF        PiInt=*on
050600160613     C                   Z-ADD     PiVal         VAOPOC
050700160613     C                   ENDIF
050800160613     C                   ENDIF
050900160613     C* NO1
051000160613     C                   IF        %subst(
051100160613     C                             %subst(vlrppt:posDaDft+1:
051200160613     C                             posADft-posDaDft-1):1:3)
051300160613     C                             = 'NO1'
051400160613     C                   EVAL      VAONO1=%trim(%subst(
051500160613     C                             %subst(vlrppt:posDaDft+1:
051600160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700160613     C                   ENDIF
051800160613     C* DDT
051900160613     C                   IF        %subst(
052000160613     C                             %subst(vlrppt:posDaDft+1:
052100160613     C                             posADft-posDaDft-1):1:3)
052200160613     C                             = 'DDT'
052300160613     C                   EVAL      VAODDT=%trim(%subst(
052400160613     C                             %subst(vlrppt:posDaDft+1:
052500160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600160613     C                   ENDIF
052700160613     C* RFA
052800160613     C                   IF        %subst(
052900160613     C                             %subst(vlrppt:posDaDft+1:
053000160613     C                             posADft-posDaDft-1):1:3)
053100160613     C                             = 'RFA'
053200160613     C                   EVAL      VAORFA=%trim(%subst(
053300160613     C                             %subst(vlrppt:posDaDft+1:
053400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500160613     C                   ENDIF
053600160613     C* FLO
053700160613     C                   IF        %subst(
053800160613     C                             %subst(vlrppt:posDaDft+1:
053900160613     C                             posADft-posDaDft-1):1:3)
054000160613     C                             = 'FLO'
054100160613     C                   EVAL      VAOFLO=%trim(%subst(
054200160613     C                             %subst(vlrppt:posDaDft+1:
054300160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400160613     C*
054500160613     C* Per sicurezza pulisco le posizioni 5, 6 e 7 del campo VAOFLO
054600160613     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
054700160613     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
054800160613     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
054900160613     C                   ENDIF
055000160613     C* ...
055100160613     C                   ENDIF
055200160613     C                   ENDDO
055300160613     C*
055400141028     C*
055500141028     C                   ENDSR
055600000801     C*----------------------------------------------------*
055700160613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE X FNVAO
055800000801     C*----------------------------------------------------*
055900160613     C     IMPVAO        BEGSR
056000040910     C*
056100040910     C* Traduzione relativa ai tipi record del file del cliente
056200040910     C*
056300071210     C*
056400071210     C***
056500160613     C* ...tipo record 'UNB' (info CMR)
056600141028     C                   EVAL      wTag = 'UNB'
056700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
056800170919     C                   MOVEL     *blanks       savKSU            7
056900141028     C                   EVAL      InStringa =
057000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
057100141028     C                   EVAL      InSepara = '+'
057200141028     C                   EXSR      SR_SPLIT
057300170919     C                   EVAL      savKSU = %trim(SkSplitCSV_5(3))
057400130620     C                   ENDIF
057500160613     C*
057600160613     C***
057700160613     C* ...tipo record 'DTM+10' (data e oara richiesta ritiro)
057800160613     C                   EVAL      wTag = 'DTM+10'
057900160613     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
058000160613     C                   Z-ADD     *zeros        savDAR            8 0
058100160613     C                   Z-ADD     *zeros        savORR            4 0
058200160613     C                   EVAL      InStringa =
058300160613     C                                %subst(wvindta:%len(%trim(wTag))+2)
058400160613     C                   EVAL      InSepara = ':'
058500160613     C                   EXSR      SR_SPLIT
058600160613     C                   SELECT
058700160613     C                   WHEN      %trim(SkSplitCSV_5(2)) = '102'
058800160613     C                   EVAL      InStringa =
058900160613     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
059000160613     C                   EVAL      PiStr=InStringa
059100160613     C                   EXSR      CHKNUM
059200160613     C                   IF        PiNum=*on
059300160613     C                   Z-ADD     PiVal         savDAR
059400160613     C                   ELSE
059500160613     C                   Z-ADD     1             wRecErr
059600160613     C                   ENDIF
059700160613     C                   WHEN      %trim(SkSplitCSV_5(2)) = '203'
059800160613     C                   EVAL      InStringa =
059900160613     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
060000160613     C                   EVAL      PiStr=InStringa
060100160613     C                   EXSR      CHKNUM
060200160613     C                   IF        PiNum=*on
060300160613     C                   Z-ADD     PiVal         savDAR
060400160613     C                   ELSE
060500160613     C                   Z-ADD     1             wRecErr
060600160613     C                   ENDIF
060700160613     C                   EVAL      InStringa =
060800160613     C                                %subst(%trim(SkSplitCSV_5(1)):9:4)
060900160613     C                   EVAL      PiStr=InStringa
061000160613     C                   EXSR      CHKNUM
061100160613     C                   IF        PiNum=*on
061200160613     C                   Z-ADD     PiVal         savORR
061300160613     C                   ELSE
061400160613     C                   Z-ADD     1             wRecErr
061500160613     C                   ENDIF
061600160613     C                   ENDSL
061700160613     C                   ENDIF
061800141028     C*
061900141028     C***
062000170919     C* ...tipo record 'BGM' (rottuta di codice ORM)
062100170919     C                   EVAL      wTag = 'BGM'
062200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062300150127     C*
062400150127     C* Scarico testata in sospeso
062500150127     C                   IF        *in40
062600160613     C  N31              ADD       1             §CTROK
062700160615     C  N31              EXSR      CHKWRI
062800170919     C  N31              EXSR      APLYPERS
062900160613     C  N31              EXSR      WRIORI
063000160613     C  N31              EXSR      WRIVGD
063100160613     C  N31              WRITE     FNVAO000
063200170919     C*
063300150127     C                   ELSE
063400150127     C                   SETON                                        40
063500150127     C                   ENDIF
063600150127     C*
063700170919     C* Inizio nuovo ORM
063800141028     C                   SETOFF                                       31
063900141028     C                   eval      vinflg = '1'
064000141028     C* ......inizializzazioni iniziali e formati record file Bartolini
064100141028     C                   EXSR      INZVAR
064200141028     C* ......valorizzazione campi fissi
064300160613     C                   EVAL      VAODAO = datcor
064400160613     C                   EVAL      VAOOAO = oracor
064500141028     C                   ENDIF
064600150127     C*
064700150127     C***
064800160613     C* ...tipo record 'NAD+CN' (Consegna)
064900150127     C                   EVAL      wTag = 'NAD+CN'
065000150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
065100160613     C                   IF        VAOCRC = *zeros
065200150127     C                   EVAL      InStringa =
065300150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
065400150127     C                   EVAL      InSepara = '+'
065500150127     C                   EXSR      SR_SPLIT
065600150921     C                   MOVEL     *blanks       wSgm2          1024
065700150921     C                   MOVEL     *blanks       wSgm4          1024
065800170919     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(3))
065900150921     C                   EVAL      wSgm4 = %trim(SkSplitCSV_5(4))
066000160613     C                   EVAL      VAOLOC = %trim(SkSplitCSV_5(5))
066100160613     C***                EVAL      VAOPRC = %trim(SkSplitCSV_5(6))
066200160613     C                   EVAL      VAOCAC = %trim(SkSplitCSV_5(7))
066300160613     C                   EVAL      VAONAC = %trim(SkSplitCSV_5(8))
066400160613     C                   IF        VAONAC = 'I'   OR
066500160613     C                             VAONAC = 'IT'  OR
066600160613     C                             VAONAC = 'ITA'
066700160613     C                   EVAL      VAONAC = *blanks
066800150127     C                   ENDIF
066900150902     C*
067000150921     C                   MOVEL     *blanks       wRSD             70
067100150921     C                   EVAL      InStringa = wSgm2
067200150902     C                   EVAL      InSepara = ':'
067300150902     C                   EXSR      SR_SPLIT
067400150921     C
067500150921     C                   EVAL      wRSD = %trim(SkSplitCSV_5(1))
067600160613     C                   EVAL      VAORSC = %subst(wRSD:1:35)
067700150902     C*
067800150921     C                   EVAL      InStringa = wSgm4
067900150127     C                   EVAL      InSepara = ':'
068000150127     C                   EXSR      SR_SPLIT
068100160209     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
068200160613     C                   EVAL      VAOINC = %trim(SkSplitCSV_5(2))
068300160209     C                   ELSE
068400160613     C                   EVAL      VAOINC = %trim(SkSplitCSV_5(1))
068500160209     C                   ENDIF
068600160613     C                   ENDIF
068700150127     C                   ENDIF
068800160613     C*
068900160613     C***
069000160613     C* ...tipo record 'NAD+CZ' (Ritiro)
069100160613     C                   EVAL      wTag = 'NAD+CZ'
069200160613     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069300160613     C                   EVAL      InStringa =
069400160613     C                                %subst(wvindta:%len(%trim(wTag))+2)
069500160613     C                   EVAL      InSepara = '+'
069600160613     C                   EXSR      SR_SPLIT
069700160613     C                   MOVEL     *blanks       wSgm2          1024
069800160613     C                   MOVEL     *blanks       wSgm4          1024
069900170919     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(3))
070000160613     C                   EVAL      wSgm4 = %trim(SkSplitCSV_5(4))
070100160613     C                   EVAL      VAOLOR = %trim(SkSplitCSV_5(5))
070200160613     C***                EVAL      VAOPRR = %trim(SkSplitCSV_5(6))
070300160613     C                   EVAL      VAOCAR = %trim(SkSplitCSV_5(7))
070400160613     C                   EVAL      VAONAR = %trim(SkSplitCSV_5(8))
070500160613     C                   IF        VAONAR = 'I'   OR
070600160613     C                             VAONAR = 'IT'  OR
070700160613     C                             VAONAR = 'ITA'
070800160613     C                   EVAL      VAONAR = *blanks
070900160613     C                   ENDIF
071000160613     C*
071100160613     C                   MOVEL     *blanks       wRSD             70
071200160613     C                   EVAL      InStringa = wSgm2
071300160613     C                   EVAL      InSepara = ':'
071400160613     C                   EXSR      SR_SPLIT
071500160613     C
071600160613     C                   EVAL      wRSD = %trim(SkSplitCSV_5(1))
071700160613     C                   EVAL      VAORSR = %subst(wRSD:1:35)
071800160613     C*
071900160613     C                   EVAL      InStringa = wSgm4
072000160613     C                   EVAL      InSepara = ':'
072100160613     C                   EXSR      SR_SPLIT
072200160613     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
072300160613     C                   EVAL      VAOINR = %trim(SkSplitCSV_5(2))
072400160615     C                   EVAL      VAONO2 = %trim(SkSplitCSV_5(1))
072500160613     C                   ELSE
072600160613     C                   EVAL      VAOINR = %trim(SkSplitCSV_5(1))
072700160613     C                   ENDIF
072800160615     C                   EVAL      VAONO2 = %trim(VAONO2)  + ' ' +
072900160613     C                                      %trim(SkSplitCSV_5(3))+
073000160613     C                                      %trim(SkSplitCSV_5(4))+
073100160613     C                                      %trim(SkSplitCSV_5(5))
073200160613     C                   ENDIF
073300150127     C*
073400150127     C***
073500170919     C* ...tipo record 'RFF+ACL' (riferimento ordine di ritiro)
073600170919     C                   EVAL      wTag = 'RFF+ACL'
073700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073800160613     C                   EVAL      VAORFA =
073900160606     C                                %trim(%subst(wvindta:%len(%trim(wTag))+2))
074000160613     C                   ENDIF
074100160613     C*
074200160613     C***
074300170919     C* ...tipo record 'RFF+AAO' (riferimento originale)
074400170919     C***                EVAL      wTag = 'RFF+AAO'
074500170919     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074600170919     C***                EVAL      VAONAM =
074700170919     C***                             %trim(%subst(wvindta:%len(%trim(wTag))+2))
074800170919     C***                ENDIF
074900170919     C*
075000160613     C* ...tipo record 'MEA+WT+G' (peso ordine di ritiro)
075100160127     C                   EVAL      wTag = 'MEA+WT+G'
075200160126     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075300160126     C                   EVAL      InStringa =
075400160126     C                                %subst(wvindta:%len(%trim(wTag))+2)
075500160126     C                   EVAL      InSepara = ':'
075600160126     C                   EXSR      SR_SPLIT
075700160126     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
075800160126     C                   EXSR      CHKNUM
075900160126     C                   IF        PiNum=*on
076000160613     C                   ADD       PiVal         VAOPKG
076100160126     C                   ELSE
076200160126     C                   Z-ADD     1             wRecErr
076300160126     C                   ENDIF
076400160126     C                   ENDIF
076500170919     C*
076600170919     C* ...tipo record 'CNT+7' (peso ordine di ritiro)
076700170919     C                   EVAL      wTag = 'CNT+7'
076800170919     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076900170919     C                   EVAL      InStringa =
077000170919     C                                %subst(wvindta:%len(%trim(wTag))+2)
077100170919     C                   EVAL      InSepara = ':'
077200170919     C                   EXSR      SR_SPLIT
077300170919     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(1))
077400170919     C                   EXSR      CHKNUM
077500170919     C                   IF        PiNum=*on
077600170919     C                   ADD       PiVal         VAOPKG
077700170919     C                   ELSE
077800170919     C                   Z-ADD     1             wRecErr
077900170919     C                   ENDIF
078000170919     C                   ENDIF
078100170919     C*
078200170919     C* ...tipo record 'CNT+11' (colli ordine di ritiro)
078300170919     C                   EVAL      wTag = 'CNT+11'
078400170919     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078500170919     C                   EVAL      InStringa =
078600170919     C                                %subst(wvindta:%len(%trim(wTag))+2)
078700170919     C                   EVAL      InSepara = ':'
078800170919     C                   EXSR      SR_SPLIT
078900170919     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(1))
079000170919     C                   EXSR      CHKNUM
079100170919     C                   IF        PiInt=*on
079200170919     C                   ADD       PiVal         VAONCL
079300170919     C                   ELSE
079400170919     C                   Z-ADD     1             wRecErr
079500170919     C                   ENDIF
079600170919     C                   ENDIF
079700170919     C*
079800170919     C***
079900170919     C* ...tipo record 'CTA+IC' (referente ritiro)
080000170919     C***                EVAL      wTag = 'CTA+IC'
080100170919     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080200170919     C***                EVAL      InStringa =
080300170919     C***                             %subst(wvindta:%len(%trim(wTag))+2)
080400170919     C***                EVAL      InSepara = ':'
080500170919     C***                EXSR      SR_SPLIT
080600170919     C***                EVAL      VAORER=%trim(SkSplitCSV_5(2))
080700170919     C***                ENDIF
080800170919     C*
080900170919     C***
081000170919     C* ...tipo record 'COM+' (telefono referente ritiro)
081100170919     C***                EVAL      wTag = 'COM+'
081200170919     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081300170919     C***                EVAL      InStringa =
081400170919     C***                             %subst(wvindta:%len(%trim(wTag))+1)
081500170919     C***                EVAL      InSepara = ':'
081600170919     C***                EXSR      SR_SPLIT
081700170919     C***                IF        %trim(SkSplitCSV_5(2)) = 'TE' AND
081800170919     C***                          %len(%trim(SkSplitCSV_5(2))) > 6
081900170919     C***                EVAL      VAOTER=%trim(SkSplitCSV_5(2))
082000170919     C***                ENDIF
082100170919     C***                ENDIF
082200141028     C*
082300141028     C***
082400160613     C* ...tipo record 'UNT' (fine ORM)
082500141028     C                   EVAL      wTag = 'UNT'
082600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082700160613     C  N31              ADD       1             §CTROK
082800160615     C  N31              EXSR      CHKWRI
082900170919     C  N31              EXSR      APLYPERS
083000160613     C  N31              EXSR      WRIORI
083100160613     C  N31              EXSR      WRIVGD
083200160613     C  N31              WRITE     FNVAO000
083300100722     C                   endif
083400010202     C*
083500000801     C* Ebbene...
083600000801     C                   ADD       1             §CTRMO
083700141028     C                   IF        wRecErr <> *zeros
083800000801     C                   ADD       1             §CTRNO
083900000801     C                   EVAL      vinflg = '2'
084000000801     C                   ENDIF
084100000801     C*
084200000801     C                   ENDSR
084300160613
084400160613
084500141028     C*----------------------------------------------------*
084600141028     C*  CONTROLLO NUMERICITA' CAMPI
084700141028     C*----------------------------------------------------*
084800141028     C     CHKNUM        BEGSR
084900141028     C*
085000141028     C                   IF        PiDecChr = *blanks
085100141028     C                   EVAL      PiDecChr = CharNUM
085200141028     C                   ENDIF
085300141028     C*
085400141028     C                   callp     UBISNUM_Check(PiStr
085500141028     C                                          :PiDecChr
085600141028     C                                          :PiVal
085700141028     C                                          :PiNum
085800141028     C                                          :PiInt)
085900141028     C*
086000141028     C                   ENDSR
086100141028     C***
086200141028
086300141028
086400141028     C*----------------------------------------------------*
086500160613     C* Splittamento stringa in array
086600160613     C*----------------------------------------------------*
086700141028     C     SR_SPLIT      BEGSR
086800141028     C*
086900141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
087000141028     C                   CALL      'XSPLIT2'
087100141028     C                   PARM                    InStringa
087200141028     C                   PARM                    InSepara
087300141028     C                   PARM      '5'           InTypeOut
087400141028     C                   PARM      ''            wSkParam
087500141028     C                   PARM                    OutErrore
087600141028     C                   PARM                    OutMsg
087700141028     C                   MOVEA     wSkParam      SkSplitCSV_5
087800141028     C*
087900141028     C                   ENDSR
088000160613
088100160613
088200160613     C*------------------------------------------------------------------------*
088300160613     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
088400160613     C*------------------------------------------------------------------------*
088500160613     C     WRIORI        BEGSR
088600160613     C*
088700160613     C* Come prima cosa stacco un numeratore da AZNUM
088800160613     C                   clear                   trul33ds
088900160613     C                   eval      I33OPE = *zeros
089000160613     C                   eval      I33CNU = 600
089100160613     C                   eval      I33NUM = 1
089200160613     C                   movel     TRUL33DS      KPJBU
089300160613     C                   call      'TRUL33R'
089400160613     C                   parm                    KPJBA
089500160613     C                   movel     KPJBU         TRUL33DS
089600160613     C                   if        O33ERR = *zeros
089700160613     C                   z-add     O33NRF        §numori
089800160613     c                   else
089900160613     c                   eval      §numori = *all'9'
090000160613     C                   endif
090100160613     C*
090200160613     C* Quindi imposto i campi "extra-VAO"
090300160613     C                   if        vlrKSC = '0ORM0000'
090400160613     C*
090500160613     C                   if        vaoCOR > *zeros
090600160613     C                   movel     vaoCOR        wrkCOR7           7 0
090700160613     C*
090800160613     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
090900160613     C* codice padre. Se è un padre assegno già il codice ordinante.
091000160613     C                   clear                   tibs10ds
091100160613     C                   eval      d10drf = datcor                              *data riferimento
091200160613     C                   eval      d10paf = 'P'                                 *cerca il padre
091300160613     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
091400160613     C*
091500160613     C* Preparazione chiamata al TIBS10R1
091600160613     C                   clear                   tibs10r1ds
091700160613     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
091800160613     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
091900160613     C                   eval      tibs10r1ds.d10igrptle = 'W'
092000160613     C*
092100160613     C                   call      'TIBS10R1'
092200160613     C                   parm                    tibs10r1ds
092300160613     C*
092400160613     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
092500160613     C*
092600160613     C                   if        d10err <> *blanks or                         *No unif. o padre
092700160613     C                             d10cop =  *zeros  or
092800160613     C                             d10cop =  d10cod
092900160613     C                   move      d10cod        oriIDC
093000160613     C                   else                                                   *Figlio
093100160613     C                   move      d10cop        oriIDC
093200160613     C                   endif
093300160613     C*
093400160613     C                   else
093500160613     C                   eval      oriIDC = *zeros
093600160613     C                   endif
093700160613     C*
093800160613     C                   else
093900160613     C                   eval      oriIDC = vlrKSC
094000160613     C                   endif
094100160613     C*
094200160613     C                   eval      oriPRG = §numori
094300160613     C                   eval      oriDER = datcor
094400160613     C                   eval      oriOER = oracor
094500160613     C                   eval      oriPOE = vaoPOE
094600160613     C                   eval      oriNSR = vaoNSR
094700160613     C                   eval      oriNOR = vaoNOR
094800160613     C                   eval      oriNRV = vaoNRV
094900160613     C                   eval      oriFLO = *blanks
095000160613     C*
095100160613     C* Infine valorizzo la chiave esterna sul file FNVAO
095200160613     C                   eval      dorm01 = vaoFLO
095300160613     C                   movel     §numori       §ormpg
095400160613     C*
095500160613     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
095600160613     C                   movel     'S'           §ormfr
095700160613     C                   movel     'S'           §ormks
095800160613     C                   movel     'S'           §ormfd
095900160613     C                   movel     sav§ONC       §orcomc
096000160613     C*
096100160613     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
096200160613     C                   eval      vaoFLO = dorm01
096300160613     C*
096400160613     C* ...quindi scrivo il file TIORI00F
096500160613     C                   WRITE     TIORI000
096600160613     C*
096700160613     C                   ENDSR
096800160613
096900160613
097000160613      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
097100160613     C     wriVGD        BEGSR
097200160613     C*
097300160613     C* Reperisco la descrizione della fase da tabella
097400160613     C                   movel(p)  'FAR'         KteCOD
097500160613     C                   movel(p)  '000'         KteKE1
097600160613     C     KEYtbe01_P    chain     tntbe01l
097700160613     C                   if        %found(tntbe01l)
097800160613     C                   movel(P)  tbeuni        DFAR
097900160613     C                   else
098000160613     C                   clear                   DFAR
098100160613     C                   endif
098200160613     C*
098300160613     C* Valorizzo buffer record
098400160613     C                   clear                   FNVAPDS
098500160613     C                   eval      vapIDC = oriIDC
098600160613     C                   eval      vapPOE = oriPOE
098700160613     C                   eval      vapRFA = vaoRFA
098800160613     C                   eval      vapPOG = oriPOE
098900160613     C                   eval      vapDAE = oriDER
099000160613     C                   eval      vapORE = oriOER
099100160613     C                   if        d§fardva <> *blanks
099200160613     C                   eval      vapDFA = d§fardva
099300160613     C                   else
099400160613     C                   eval      vapDFA = d§fardes
099500160613     C                   endif
099600160613     C*
099700160613     C                   clear                   tivgd000
099800160613     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
099900160613     C                   eval      vgdTIP = 'VP'
100000160613     C                   eval      vgdKSU = vapIDC
100100160613     C                   eval      vgdTSC = 'WW'
100200160613     C                   eval      vgdDAT = datcor
100300170919     C                   eval      vgdPGM = 'TITVED33R'
100400160613     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
100500160613     C                   exsr      chkVAP
100600160613     C                   if        Cli_Abil = *on
100700160613     C                   eval      vgdKSU = woriIDC
100800160613     C                   write     tivgd000
100900160613     C                   endif
101000160613     C*
101100160613     C                   ENDSR
101200160613     C*------------------------------------------------------------------------*
101300160613
101400160613
101500160613     C*-------------------------------------------------------------------------*
101600160613     C* Controllo abilitazione cliente al ritorno fasi ORM
101700160613     C*-------------------------------------------------------------------------*
101800160613     C     chkVAP        begsr
101900160613     C*
102000160613     C* Inizializzo a OK il flag di controllo abilitazione
102100160613     C                   eval      Cli_Abil = *on
102200160613     C
102300160613     C* Controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
102400160613     C                   clear                   woriIDC
102500160613     C                   eval      kteCOD = 'VAP'
102600160613     C                   eval      kteKE1 = oriIDC
102700160613     C     KEYtbe01_P    chain     TNTBE01L
102800160613     C                   if        %found(Tntbe01l)
102900160613     C                   eval      woriIDC = tbeKE2
103000160613     C                   else
103100160613     C                   eval      Cli_Abil = *off
103200160613     C                   endif
103300160613     C*
103400160613     C                   endsr
103500160613
103600160613
103700160613      /TITLE Invio dei dati al punto operativo.
103800160613     C     invio         BEGSR
103900160613     C*
104000160613     C                   reset                   dscmz
104100160613     C                   move      vlrpoi        cmzdst
104200160613     C                   eval      cmzfld = vlrfou
104300160613     C                   eval      cmzmbd = vlrhdl
104400160613     C                   eval      %subst(cmzmbd:1:1) = 'M'
104500160613     C                   if        prmfir = *blanks
104600160613     C                   eval      cmzfla = 'FNVAO00F'
104700160613     C                   eval      cmzmba = 'FNVAO00F'
104800160613     C                   else
104900160613     C                   eval      cmzfla = prmfir
105000160613     C                   eval      cmzmba = prmfir
105100160613     C                   endif
105200160613     C                   eval      cmznrr = *zeros
105300160613     C                   move      §ctrok        cmznrr
105400160613     C                   eval      cmzlba = vlrfl1
105500160613     C                   call(e)   'TIS711C'
105600160613     C                   parm                    dscmz
105700160613     C                   parm      *blanks       esito
105800160613     C                   if        %error
105900160613     C                             or cmzerr = '1'
106000160613     C                             or esito  = '1'
106100160613     C                   eval      wrkesito = '3'
106200160613     C                   endif
106300160613     C*
106400160613     C                   ENDSR
106500070411
106600141028
106700070411     C     *pssr         BEGSR
106800070411     C*
106900070411     C                   if        %open(tivin00r)
107000070411     C                   close     tivin00r
107100070411     C                   endif
107200160613     C                   if        %open(fnvaowwr)
107300160613     C                   close     fnvaowwr
107400070411     C                   endif
107500070411     C*
107600070411     C                   eval      wrkesito = '2'
107700070411     C*
107800070411     C                   seton                                        LR
107900070411     C*
108000070411     C                   ENDSR     '*CANCL'
108100070411     C***
108200070411
108300990910
108400000613     C     *inzsr        BEGSR
108500990910     C*
108600990910     C     *entry        plist
108700990920     C                   parm                    tivlrds
108800990921     C                   parm      wrkesito      esito
108900000724     C                   parm                    prmlit
109000000710     C                   parm                    prmfir
109100000613     C*
109200000830     C* CALCOLA LA DATA CORRENTE
109300140117     C                   time                    wn14             14 0
109400140117     C                   movel     wn14          oracor            6 0          *ORA
109500100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
109600100722     C                   eval      datcor = %dec(%date() : *ISO)
109700160613     C*
109800160613     C* Campi riferiti al database
109900160613     C     *like         define    tbecod        kteCOD                         *TNTBE01L
110000160613     C     *like         define    tbeke1        kteKE1
110100160613     C*
110200160613     C* Definizione chiavi di lettura
110300160613     C*
110400160613     C* TNTBE01L - Parziale
110500160613     C     KEYtbe01_P    klist
110600160613     C                   kfld                    kteCOD                         *UTENTE
110700160613     C                   kfld                    kteKE1                         *CODICE
110800000830     C*
110900000613     C                   ENDSR
111000000613     C***
