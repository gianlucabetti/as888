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
001800160613     D fnvapds       e ds                  extname(fnvap00f)
001900160613     D dorm01        e ds
002000160613     D dfar          e ds
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
025300160615 xxx C     APLYPERS      BEGSR
025400160615     C*
025500160615     C* ......VAONO1
025600160615     C                   eval      VAONO1 = 'CODICE APERTURA LOCKER: ' + VAORFA
025700160615     C*
025800160615     C                   ENDSR
025900160615
026000160615
026100141028     C*----------------------------------------------------*
026200141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300141028     C*----------------------------------------------------*
026400141028     C     INZVAR        BEGSR
026500141028     C*
026600141028     C* Inizializzo variabili di wrk
026700141028     C                   Z-ADD     *zeros        Num5_0            5 0
026800160613     C                   Z-ADD     *zeros        cntVAONCL         5 0
026900141028     C                   CLEAR                   SkSplitCSV_5
027000141028     C*
027100141028     C* Reimposto i valori di default
027200141028     C                   EXSR      DEFCAM
027300141028     C*
027400141028     C                   ENDSR
027500141028     C*----------------------------------------------------*
027600141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
027700141028     C*----------------------------------------------------*
027800141028     C     DEFCAM        BEGSR
027900141028     C*
028000160613     C                   CLEAR                   FNVAO000
028100160613     C                   CLEAR                   TIORI000
028200141028     C*
028300160613     C* Reperisco se elaborazione in TEST (*IN55) o PROD (not *IN55)
028400160613     C                   SETOFF                                       55
028500160613     C                   IF        %subst(vlrppt:1:1)='T'
028600160613     C                   SETON                                        55
028700160613     C                   ENDIF
028800160613     C*
028900160613     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029000160613     C* e delimitatore testo.
029100160613     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029200141028     C*
029300141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
029400141028     C                   EVAL      CharSOS = CharNUM
029500160613     C*
029600160613     C* Imposto i valori di default
029700160613     C                   CLEAR                   DORM01
029800160613     C                   EVAL      VAOFLO = DORM01
029900160613     C                   EVAL      VAOTCO = 'F'
030000160613     C                   EVAL      VAOTOR = 'S'
030100160615     C                   EVAL      VAOPAG = 'O'
030200160615     C                   EVAL      VAOPOE = vlrpoi
030300160615     C*
030400160613     C* IMPOSTO I CAMPI PERSONALIZZATI
030500160613     C*
030600160613     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030700160613     C                   EVAL      posDaDft = 1
030800160613     C                   EVAL      posADft  = 0
030900160613     C                   EVAL      wGiroDft = 0
031000160613     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031100160613     C                             posDaDft > 0
031200160613     C*
031300160613     C* Gestisco il 1° giro
031400160613     C                   IF        wGiroDft = 0
031500160613     C* Eseguo lo scan x trovare l'inizio del campo corrente
031600160613     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031700160613     C* Incremento il contatore dei "giri"
031800160613     C                   EVAL      wGiroDft = 1
031900160613     C                   ELSE
032000160613     C                   EVAL      posDaDft = posADft
032100160613     C                   ENDIF
032200160613     C* Eseguo lo scan x trovare la fine del campo corrente
032300160613     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032400160613     C*
032500160613     C* A questo "estraggo" il parametro (campo e valore) corrente...
032600160613     C* ...solo se entrambe le posizini (DA/A) sono > 0
032700160613     C                   IF        posDaDft > 0 AND
032800160613     C                             posADft  > 0
032900160613     C* §ONC (ORM Non Commissionato)
033000160613     C                   IF        %subst(
033100160613     C                             %subst(vlrppt:posDaDft+1:
033200160613     C                             posADft-posDaDft-1):1:4)
033300160613     C                             = '§ONC'
033400160613     C                   MOVEL     *blanks       sav§ONC           1
033500160613     C                   EVAL      sav§ONC=%trim(%subst(
033600160613     C                             %subst(vlrppt:posDaDft+1:
033700160613     C                             posADft-posDaDft-1):5:posADft-posDaDft-1-4))
033800160613     C                   ENDIF
033900160613     C* POE
034000160613     C                   IF        %subst(
034100160613     C                             %subst(vlrppt:posDaDft+1:
034200160613     C                             posADft-posDaDft-1):1:3)
034300160613     C                             = 'POE'
034400160613     C                   EVAL      PiStr=%trim(%subst(
034500160613     C                             %subst(vlrppt:posDaDft+1:
034600160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034700160613     C                   EXSR      CHKNUM
034800160613     C                   IF        PiInt=*on
034900160613     C                   Z-ADD     PiVal         VAOPOE
035000160613     C                   ENDIF
035100160613     C                   ENDIF
035200160613     C* TOR
035300160613     C                   IF        %subst(
035400160613     C                             %subst(vlrppt:posDaDft+1:
035500160613     C                             posADft-posDaDft-1):1:3)
035600160613     C                             = 'TOR'
035700160613     C                   EVAL      VAOTOR=%trim(%subst(
035800160613     C                             %subst(vlrppt:posDaDft+1:
035900160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036000160613     C                   ENDIF
036100160613     C* TCO
036200160613     C                   IF        %subst(
036300160613     C                             %subst(vlrppt:posDaDft+1:
036400160613     C                             posADft-posDaDft-1):1:3)
036500160613     C                             = 'TCO'
036600160613     C                   EVAL      VAOTCO=%trim(%subst(
036700160613     C                             %subst(vlrppt:posDaDft+1:
036800160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036900160613     C                   ENDIF
037000160613     C* COR
037100160613     C                   IF        %subst(
037200160613     C                             %subst(vlrppt:posDaDft+1:
037300160613     C                             posADft-posDaDft-1):1:3)
037400160613     C                             = 'COR'
037500160613     C                   EVAL      PiStr=%trim(%subst(
037600160613     C                             %subst(vlrppt:posDaDft+1:
037700160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800160613     C                   EXSR      CHKNUM
037900160613     C                   IF        PiInt=*on
038000160613     C                   Z-ADD     PiVal         VAOCOR
038100160613     C                   ENDIF
038200160613     C                   ENDIF
038300160613     C* CRC
038400160613     C                   IF        %subst(
038500160613     C                             %subst(vlrppt:posDaDft+1:
038600160613     C                             posADft-posDaDft-1):1:3)
038700160613     C                             = 'CRC'
038800160613     C                   EVAL      PiStr=%trim(%subst(
038900160613     C                             %subst(vlrppt:posDaDft+1:
039000160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100160613     C                   EXSR      CHKNUM
039200160613     C                   IF        PiInt=*on
039300160613     C                   Z-ADD     PiVal         VAOCRC
039400160613     C                   ENDIF
039500160613     C                   ENDIF
039600160613     C* FFD
039700160613     C                   IF        %subst(
039800160613     C                             %subst(vlrppt:posDaDft+1:
039900160613     C                             posADft-posDaDft-1):1:3)
040000160613     C                             = 'FFD'
040100160613     C                   EVAL      VAOFFD=%trim(%subst(
040200160613     C                             %subst(vlrppt:posDaDft+1:
040300160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040400160613     C                   ENDIF
040500160613     C* ORR
040600160613     C                   IF        %subst(
040700160613     C                             %subst(vlrppt:posDaDft+1:
040800160613     C                             posADft-posDaDft-1):1:3)
040900160613     C                             = 'ORR'
041000160613     C                   EVAL      PiStr=%trim(%subst(
041100160613     C                             %subst(vlrppt:posDaDft+1:
041200160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041300160613     C                   EXSR      CHKNUM
041400160613     C                   IF        PiInt=*on
041500160613     C                   Z-ADD     PiVal         VAOORR
041600160613     C                   ENDIF
041700160613     C                   ENDIF
041800160613     C* NCL
041900160613     C                   IF        %subst(
042000160613     C                             %subst(vlrppt:posDaDft+1:
042100160613     C                             posADft-posDaDft-1):1:3)
042200160613     C                             = 'NCL'
042300160613     C                   EVAL      PiStr=%trim(%subst(
042400160613     C                             %subst(vlrppt:posDaDft+1:
042500160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042600160613     C                   EXSR      CHKNUM
042700160613     C                   IF        PiInt=*on
042800160613     C                   Z-ADD     PiVal         VAONCL
042900160613     C                   ENDIF
043000160613     C                   ENDIF
043100160613     C* PKG
043200160613     C                   IF        %subst(
043300160613     C                             %subst(vlrppt:posDaDft+1:
043400160613     C                             posADft-posDaDft-1):1:3)
043500160613     C                             = 'PKG'
043600160613     C                   EVAL      PiStr=%trim(%subst(
043700160613     C                             %subst(vlrppt:posDaDft+1:
043800160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043900160613     C                   EXSR      CHKNUM
044000160613     C                   IF        PiNum=*on
044100160613     C                   Z-ADD     PiVal         VAOPKG
044200160613     C                   ENDIF
044300160613     C                   ENDIF
044400160613     C* VLM
044500160613     C                   IF        %subst(
044600160613     C                             %subst(vlrppt:posDaDft+1:
044700160613     C                             posADft-posDaDft-1):1:3)
044800160613     C                             = 'VLM'
044900160613     C                   EVAL      PiStr=%trim(%subst(
045000160613     C                             %subst(vlrppt:posDaDft+1:
045100160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045200160613     C                   EXSR      CHKNUM
045300160613     C                   IF        PiNum=*on
045400160613     C                   Z-ADD     PiVal         VAOVLM
045500160613     C                   ENDIF
045600160613     C                   ENDIF
045700160613     C* NAM
045800160613     C                   IF        %subst(
045900160613     C                             %subst(vlrppt:posDaDft+1:
046000160613     C                             posADft-posDaDft-1):1:3)
046100160613     C                             = 'NAM'
046200160613     C                   EVAL      VAONAM=%trim(%subst(
046300160613     C                             %subst(vlrppt:posDaDft+1:
046400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046500160613     C                   ENDIF
046600160613     C* PAG
046700160613     C                   IF        %subst(
046800160613     C                             %subst(vlrppt:posDaDft+1:
046900160613     C                             posADft-posDaDft-1):1:3)
047000160613     C                             = 'PAG'
047100160613     C                   EVAL      VAOPAG=%trim(%subst(
047200160613     C                             %subst(vlrppt:posDaDft+1:
047300160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047400160613     C                   ENDIF
047500160613     C* KSC
047600160613     C                   IF        %subst(
047700160613     C                             %subst(vlrppt:posDaDft+1:
047800160613     C                             posADft-posDaDft-1):1:3)
047900160613     C                             = 'KSC'
048000160613     C                   EVAL      PiStr=%trim(%subst(
048100160613     C                             %subst(vlrppt:posDaDft+1:
048200160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048300160613     C                   EXSR      CHKNUM
048400160613     C                   IF        PiInt=*on
048500160613     C                   Z-ADD     PiVal         VAOKSC
048600160613     C                   ENDIF
048700160613     C                   ENDIF
048800160613     C* CTR
048900160613     C                   IF        %subst(
049000160613     C                             %subst(vlrppt:posDaDft+1:
049100160613     C                             posADft-posDaDft-1):1:3)
049200160613     C                             = 'CTR'
049300160613     C                   EVAL      VAOCTR=%trim(%subst(
049400160613     C                             %subst(vlrppt:posDaDft+1:
049500160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049600160613     C                   ENDIF
049700160613     C* POC
049800160613     C                   IF        %subst(
049900160613     C                             %subst(vlrppt:posDaDft+1:
050000160613     C                             posADft-posDaDft-1):1:3)
050100160613     C                             = 'POC'
050200160613     C                   EVAL      PiStr=%trim(%subst(
050300160613     C                             %subst(vlrppt:posDaDft+1:
050400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050500160613     C                   EXSR      CHKNUM
050600160613     C                   IF        PiInt=*on
050700160613     C                   Z-ADD     PiVal         VAOPOC
050800160613     C                   ENDIF
050900160613     C                   ENDIF
051000160613     C* NO1
051100160613     C                   IF        %subst(
051200160613     C                             %subst(vlrppt:posDaDft+1:
051300160613     C                             posADft-posDaDft-1):1:3)
051400160613     C                             = 'NO1'
051500160613     C                   EVAL      VAONO1=%trim(%subst(
051600160613     C                             %subst(vlrppt:posDaDft+1:
051700160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800160613     C                   ENDIF
051900160613     C* DDT
052000160613     C                   IF        %subst(
052100160613     C                             %subst(vlrppt:posDaDft+1:
052200160613     C                             posADft-posDaDft-1):1:3)
052300160613     C                             = 'DDT'
052400160613     C                   EVAL      VAODDT=%trim(%subst(
052500160613     C                             %subst(vlrppt:posDaDft+1:
052600160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700160613     C                   ENDIF
052800160613     C* RFA
052900160613     C                   IF        %subst(
053000160613     C                             %subst(vlrppt:posDaDft+1:
053100160613     C                             posADft-posDaDft-1):1:3)
053200160613     C                             = 'RFA'
053300160613     C                   EVAL      VAORFA=%trim(%subst(
053400160613     C                             %subst(vlrppt:posDaDft+1:
053500160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600160613     C                   ENDIF
053700160613     C* FLO
053800160613     C                   IF        %subst(
053900160613     C                             %subst(vlrppt:posDaDft+1:
054000160613     C                             posADft-posDaDft-1):1:3)
054100160613     C                             = 'FLO'
054200160613     C                   EVAL      VAOFLO=%trim(%subst(
054300160613     C                             %subst(vlrppt:posDaDft+1:
054400160613     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500160613     C*
054600160613     C* Per sicurezza pulisco le posizioni 5, 6 e 7 del campo VAOFLO
054700160613     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
054800160613     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
054900160613     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
055000160613     C                   ENDIF
055100160613     C* ...
055200160613     C                   ENDIF
055300160613     C                   ENDDO
055400160613     C*
055500141028     C*
055600141028     C                   ENDSR
055700000801     C*----------------------------------------------------*
055800160613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE X FNVAO
055900000801     C*----------------------------------------------------*
056000160613     C     IMPVAO        BEGSR
056100040910     C*
056200040910     C* Traduzione relativa ai tipi record del file del cliente
056300040910     C*
056400071210     C*
056500071210     C***
056600160613     C* ...tipo record 'UNB' (info CMR)
056700141028     C                   EVAL      wTag = 'UNB'
056800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
056900160613     C                   MOVEL     *blanks       savCMR           35
057000141028     C                   EVAL      InStringa =
057100141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
057200141028     C                   EVAL      InSepara = '+'
057300141028     C                   EXSR      SR_SPLIT
057400160613     C                   EVAL      savCMR = 'CMR: ' + %trim(SkSplitCSV_5(5))
057500130620     C                   ENDIF
057600160613     C*
057700160613     C***
057800160613     C* ...tipo record 'DTM+10' (data e oara richiesta ritiro)
057900160613     C                   EVAL      wTag = 'DTM+10'
058000160613     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
058100160613     C                   Z-ADD     *zeros        savDAR            8 0
058200160613     C                   Z-ADD     *zeros        savORR            4 0
058300160613     C                   EVAL      InStringa =
058400160613     C                                %subst(wvindta:%len(%trim(wTag))+2)
058500160613     C                   EVAL      InSepara = ':'
058600160613     C                   EXSR      SR_SPLIT
058700160613     C                   SELECT
058800160613     C                   WHEN      %trim(SkSplitCSV_5(2)) = '102'
058900160613     C                   EVAL      InStringa =
059000160613     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
059100160613     C                   EVAL      PiStr=InStringa
059200160613     C                   EXSR      CHKNUM
059300160613     C                   IF        PiNum=*on
059400160613     C                   Z-ADD     PiVal         savDAR
059500160613     C                   ELSE
059600160613     C                   Z-ADD     1             wRecErr
059700160613     C                   ENDIF
059800160613     C                   WHEN      %trim(SkSplitCSV_5(2)) = '203'
059900160613     C                   EVAL      InStringa =
060000160613     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
060100160613     C                   EVAL      PiStr=InStringa
060200160613     C                   EXSR      CHKNUM
060300160613     C                   IF        PiNum=*on
060400160613     C                   Z-ADD     PiVal         savDAR
060500160613     C                   ELSE
060600160613     C                   Z-ADD     1             wRecErr
060700160613     C                   ENDIF
060800160613     C                   EVAL      InStringa =
060900160613     C                                %subst(%trim(SkSplitCSV_5(1)):9:4)
061000160613     C                   EVAL      PiStr=InStringa
061100160613     C                   EXSR      CHKNUM
061200160613     C                   IF        PiNum=*on
061300160613     C                   Z-ADD     PiVal         savORR
061400160613     C                   ELSE
061500160613     C                   Z-ADD     1             wRecErr
061600160613     C                   ENDIF
061700160613     C                   ENDSL
061800160613     C                   ENDIF
061900141028     C*
062000141028     C***
062100160613     C* ...tipo record 'GID' (rottuta di codice ORM)
062200150127     C                   EVAL      wTag = 'GID'
062300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062400150127     C*
062500150127     C* Scarico testata in sospeso
062600150127     C                   IF        *in40
062700160613     C  N31              ADD       1             §CTROK
062800160615     C  N31              EXSR      CHKWRI
062900160615 xxx C  N31              EXSR      APLYPERS
063000160613     C  N31              EXSR      WRIORI
063100160613     C  N31              EXSR      WRIVGD
063200160613     C  N31              WRITE     FNVAO000
063300150127     C                   ELSE
063400150127     C                   SETON                                        40
063500150127     C                   ENDIF
063600150127     C*
063700150127     C* Inizio nuova bolla
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
065800150921     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(2))
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
069900160613     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(2))
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
073500160613     C* ...tipo record 'RFF+CR' (riferimento ordine di ritiro)
073600160613     C                   EVAL      wTag = 'RFF+CR'
073700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073800160613     C                   EVAL      VAORFA =
073900160606     C                                %trim(%subst(wvindta:%len(%trim(wTag))+2))
074000160613     C* Ad ogni riferimento (password loacker) conteggio anche 1 collo
074100160613     C                   ADD       1             cntVAONCL
074200160613     C                   ENDIF
074300160613     C*
074400160613     C***
074500160613     C* ...tipo record 'RFF+ZZZ' (riferimento originale)
074600160613     C                   EVAL      wTag = 'RFF+ZZZ'
074700160613     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074800160613     C                   EVAL      VAONAM =
074900160613     C                                %trim(%subst(wvindta:%len(%trim(wTag))+2))
075000160613     C                   ENDIF
075100160126     C*
075200160126     C***
075300160613     C* ...tipo record 'MEA+WT+G' (peso ordine di ritiro)
075400160127     C                   EVAL      wTag = 'MEA+WT+G'
075500160126     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075600160126     C                   EVAL      InStringa =
075700160126     C                                %subst(wvindta:%len(%trim(wTag))+2)
075800160126     C                   EVAL      InSepara = ':'
075900160126     C                   EXSR      SR_SPLIT
076000160126     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
076100160126     C                   EXSR      CHKNUM
076200160126     C                   IF        PiNum=*on
076300160613     C                   ADD       PiVal         VAOPKG
076400160613     C                   IF        VAOPKG = *zeros
076500160613     C                   EVAL      VAOPKG = 0,1
076600160128     C                   ENDIF
076700160126     C                   ELSE
076800160126     C                   Z-ADD     1             wRecErr
076900160126     C                   ENDIF
077000160126     C                   ENDIF
077100141028     C*
077200141028     C***
077300160613     C* ...tipo record 'UNT' (fine ORM)
077400141028     C                   EVAL      wTag = 'UNT'
077500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077600160613     C  N31              ADD       1             §CTROK
077700160615     C  N31              EXSR      CHKWRI
077800160615 xxx C  N31              EXSR      APLYPERS
077900160613     C  N31              EXSR      WRIORI
078000160613     C  N31              EXSR      WRIVGD
078100160613     C  N31              WRITE     FNVAO000
078200100722     C                   endif
078300010202     C*
078400000801     C* Ebbene...
078500000801     C                   ADD       1             §CTRMO
078600141028     C                   IF        wRecErr <> *zeros
078700000801     C                   ADD       1             §CTRNO
078800000801     C                   EVAL      vinflg = '2'
078900000801     C                   ENDIF
079000000801     C*
079100000801     C                   ENDSR
079200160613
079300160613
079400141028     C*----------------------------------------------------*
079500141028     C*  CONTROLLO NUMERICITA' CAMPI
079600141028     C*----------------------------------------------------*
079700141028     C     CHKNUM        BEGSR
079800141028     C*
079900141028     C                   IF        PiDecChr = *blanks
080000141028     C                   EVAL      PiDecChr = CharNUM
080100141028     C                   ENDIF
080200141028     C*
080300141028     C                   callp     UBISNUM_Check(PiStr
080400141028     C                                          :PiDecChr
080500141028     C                                          :PiVal
080600141028     C                                          :PiNum
080700141028     C                                          :PiInt)
080800141028     C*
080900141028     C                   ENDSR
081000141028     C***
081100141028
081200141028
081300141028     C*----------------------------------------------------*
081400160613     C* Splittamento stringa in array
081500160613     C*----------------------------------------------------*
081600141028     C     SR_SPLIT      BEGSR
081700141028     C*
081800141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
081900141028     C                   CALL      'XSPLIT2'
082000141028     C                   PARM                    InStringa
082100141028     C                   PARM                    InSepara
082200141028     C                   PARM      '5'           InTypeOut
082300141028     C                   PARM      ''            wSkParam
082400141028     C                   PARM                    OutErrore
082500141028     C                   PARM                    OutMsg
082600141028     C                   MOVEA     wSkParam      SkSplitCSV_5
082700141028     C*
082800141028     C                   ENDSR
082900160613
083000160613
083100160613     C*------------------------------------------------------------------------*
083200160613     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
083300160613     C*------------------------------------------------------------------------*
083400160613     C     WRIORI        BEGSR
083500160613     C*
083600160613     C* Come prima cosa stacco un numeratore da AZNUM
083700160613     C                   clear                   trul33ds
083800160613     C                   eval      I33OPE = *zeros
083900160613     C                   eval      I33CNU = 600
084000160613     C                   eval      I33NUM = 1
084100160613     C                   movel     TRUL33DS      KPJBU
084200160613     C                   call      'TRUL33R'
084300160613     C                   parm                    KPJBA
084400160613     C                   movel     KPJBU         TRUL33DS
084500160613     C                   if        O33ERR = *zeros
084600160613     C                   z-add     O33NRF        §numori
084700160613     c                   else
084800160613     c                   eval      §numori = *all'9'
084900160613     C                   endif
085000160613     C*
085100160613     C* Quindi imposto i campi "extra-VAO"
085200160613     C                   if        vlrKSC = '0ORM0000'
085300160613     C*
085400160613     C                   if        vaoCOR > *zeros
085500160613     C                   movel     vaoCOR        wrkCOR7           7 0
085600160613     C*
085700160613     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
085800160613     C* codice padre. Se è un padre assegno già il codice ordinante.
085900160613     C                   clear                   tibs10ds
086000160613     C                   eval      d10drf = datcor                              *data riferimento
086100160613     C                   eval      d10paf = 'P'                                 *cerca il padre
086200160613     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
086300160613     C*
086400160613     C* Preparazione chiamata al TIBS10R1
086500160613     C                   clear                   tibs10r1ds
086600160613     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
086700160613     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
086800160613     C                   eval      tibs10r1ds.d10igrptle = 'W'
086900160613     C*
087000160613     C                   call      'TIBS10R1'
087100160613     C                   parm                    tibs10r1ds
087200160613     C*
087300160613     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
087400160613     C*
087500160613     C                   if        d10err <> *blanks or                         *No unif. o padre
087600160613     C                             d10cop =  *zeros  or
087700160613     C                             d10cop =  d10cod
087800160613     C                   move      d10cod        oriIDC
087900160613     C                   else                                                   *Figlio
088000160613     C                   move      d10cop        oriIDC
088100160613     C                   endif
088200160613     C*
088300160613     C                   else
088400160613     C                   eval      oriIDC = *zeros
088500160613     C                   endif
088600160613     C*
088700160613     C                   else
088800160613     C                   eval      oriIDC = vlrKSC
088900160613     C                   endif
089000160613     C*
089100160613     C                   eval      oriPRG = §numori
089200160613     C                   eval      oriDER = datcor
089300160613     C                   eval      oriOER = oracor
089400160613     C                   eval      oriPOE = vaoPOE
089500160613     C                   eval      oriNSR = vaoNSR
089600160613     C                   eval      oriNOR = vaoNOR
089700160613     C                   eval      oriNRV = vaoNRV
089800160613     C                   eval      oriFLO = *blanks
089900160613     C*
090000160613     C* Infine valorizzo la chiave esterna sul file FNVAO
090100160613     C                   eval      dorm01 = vaoFLO
090200160613     C                   movel     §numori       §ormpg
090300160613     C*
090400160613     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
090500160613     C                   movel     'S'           §ormfr
090600160613     C                   movel     'S'           §ormks
090700160613     C                   movel     'S'           §ormfd
090800160613     C                   movel     sav§ONC       §orcomc
090900160613     C*
091000160613     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
091100160613     C                   eval      vaoFLO = dorm01
091200160613     C*
091300160613     C* ...quindi scrivo il file TIORI00F
091400160613     C                   WRITE     TIORI000
091500160613     C*
091600160613     C                   ENDSR
091700160613
091800160613
091900160613      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
092000160613     C     wriVGD        BEGSR
092100160613     C*
092200160613     C* Reperisco la descrizione della fase da tabella
092300160613     C                   movel(p)  'FAR'         KteCOD
092400160613     C                   movel(p)  '000'         KteKE1
092500160613     C     KEYtbe01_P    chain     tntbe01l
092600160613     C                   if        %found(tntbe01l)
092700160613     C                   movel(P)  tbeuni        DFAR
092800160613     C                   else
092900160613     C                   clear                   DFAR
093000160613     C                   endif
093100160613     C*
093200160613     C* Valorizzo buffer record
093300160613     C                   clear                   FNVAPDS
093400160613     C                   eval      vapIDC = oriIDC
093500160613     C                   eval      vapPOE = oriPOE
093600160613     C                   eval      vapRFA = vaoRFA
093700160613     C                   eval      vapPOG = oriPOE
093800160613     C                   eval      vapDAE = oriDER
093900160613     C                   eval      vapORE = oriOER
094000160613     C                   if        d§fardva <> *blanks
094100160613     C                   eval      vapDFA = d§fardva
094200160613     C                   else
094300160613     C                   eval      vapDFA = d§fardes
094400160613     C                   endif
094500160613     C*
094600160613     C                   clear                   tivgd000
094700160613     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
094800160613     C                   eval      vgdTIP = 'VP'
094900160613     C                   eval      vgdKSU = vapIDC
095000160613     C                   eval      vgdTSC = 'WW'
095100160613     C                   eval      vgdDAT = datcor
095200160613     C                   eval      vgdPGM = 'TITVED15R'
095300160613     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
095400160613     C                   exsr      chkVAP
095500160613     C                   if        Cli_Abil = *on
095600160613     C                   eval      vgdKSU = woriIDC
095700160613     C                   write     tivgd000
095800160613     C                   endif
095900160613     C*
096000160613     C                   ENDSR
096100160613     C*------------------------------------------------------------------------*
096200160613
096300160613
096400160613     C*-------------------------------------------------------------------------*
096500160613     C* Controllo abilitazione cliente al ritorno fasi ORM
096600160613     C*-------------------------------------------------------------------------*
096700160613     C     chkVAP        begsr
096800160613     C*
096900160613     C* Inizializzo a OK il flag di controllo abilitazione
097000160613     C                   eval      Cli_Abil = *on
097100160613     C
097200160613     C* Controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
097300160613     C                   clear                   woriIDC
097400160613     C                   eval      kteCOD = 'VAP'
097500160613     C                   eval      kteKE1 = oriIDC
097600160613     C     KEYtbe01_P    chain     TNTBE01L
097700160613     C                   if        %found(Tntbe01l)
097800160613     C                   eval      woriIDC = tbeKE2
097900160613     C                   else
098000160613     C                   eval      Cli_Abil = *off
098100160613     C                   endif
098200160613     C*
098300160613     C                   endsr
098400160613
098500160613
098600160613      /TITLE Invio dei dati al punto operativo.
098700160613     C     invio         BEGSR
098800160613     C*
098900160613     C                   reset                   dscmz
099000160613     C                   move      vlrpoi        cmzdst
099100160613     C                   eval      cmzfld = vlrfou
099200160613     C                   eval      cmzmbd = vlrhdl
099300160613     C                   eval      %subst(cmzmbd:1:1) = 'M'
099400160613     C                   if        prmfir = *blanks
099500160613     C                   eval      cmzfla = 'FNVAO00F'
099600160613     C                   eval      cmzmba = 'FNVAO00F'
099700160613     C                   else
099800160613     C                   eval      cmzfla = prmfir
099900160613     C                   eval      cmzmba = prmfir
100000160613     C                   endif
100100160613     C                   eval      cmznrr = *zeros
100200160613     C                   move      §ctrok        cmznrr
100300160613     C                   eval      cmzlba = vlrfl1
100400160613     C                   call(e)   'TIS711C'
100500160613     C                   parm                    dscmz
100600160613     C                   parm      *blanks       esito
100700160613     C                   if        %error
100800160613     C                             or cmzerr = '1'
100900160613     C                             or esito  = '1'
101000160613     C                   eval      wrkesito = '3'
101100160613     C                   endif
101200160613     C*
101300160613     C                   ENDSR
101400070411
101500141028
101600070411     C     *pssr         BEGSR
101700070411     C*
101800070411     C                   if        %open(tivin00r)
101900070411     C                   close     tivin00r
102000070411     C                   endif
102100160613     C                   if        %open(fnvaowwr)
102200160613     C                   close     fnvaowwr
102300070411     C                   endif
102400070411     C*
102500070411     C                   eval      wrkesito = '2'
102600070411     C*
102700070411     C                   seton                                        LR
102800070411     C*
102900070411     C                   ENDSR     '*CANCL'
103000070411     C***
103100070411
103200990910
103300000613     C     *inzsr        BEGSR
103400990910     C*
103500990910     C     *entry        plist
103600990920     C                   parm                    tivlrds
103700990921     C                   parm      wrkesito      esito
103800000724     C                   parm                    prmlit
103900000710     C                   parm                    prmfir
104000000613     C*
104100000830     C* CALCOLA LA DATA CORRENTE
104200140117     C                   time                    wn14             14 0
104300140117     C                   movel     wn14          oracor            6 0          *ORA
104400100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
104500100722     C                   eval      datcor = %dec(%date() : *ISO)
104600160613     C*
104700160613     C* Campi riferiti al database
104800160613     C     *like         define    tbecod        kteCOD                         *TNTBE01L
104900160613     C     *like         define    tbeke1        kteKE1
105000160613     C*
105100160613     C* Definizione chiavi di lettura
105200160613     C*
105300160613     C* TNTBE01L - Parziale
105400160613     C     KEYtbe01_P    klist
105500160613     C                   kfld                    kteCOD                         *UTENTE
105600160613     C                   kfld                    kteKE1                         *CODICE
105700000830     C*
105800000613     C                   ENDSR
105900000613     C***
