000100130503      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
000400030624     Ffnacr01l  if   e           k disk
000500090617     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700010122     FFNVAOwwr  O    E             DISK    usropn
000800011105     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000990908
001100030624     D*------------------
001200030624     D* DS REPERIMENTO DATI CLIENTE
001300030624     D*-------------------
001400030624     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001500030624     D ACODS         E DS                  EXTNAME(CNACO00F)
001600030624     D INDDS         E DS                  EXTNAME(CNIND00F)
001700030624     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001800030624     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300010122     D*
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500050826     D fnvapds       e ds                  extname(fnvap00f)
002600071016     D tisi95ds      e ds
002700011105     D dorm01        e ds
002800011119     D dfar          e ds
002900110502     D trul33ds      e ds
003000110502     D kpjba         e ds
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600110502     D §numori         s              7  0 inz
003700160916     D depORM          s             20    inz
003800011122     D*-------------------
003900010731     D* COSTANTI
004000011122     D*-------------------
004100010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004200010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004300990908
004400010528
004500010528
004600010528
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000000724     C*
005100010528     C                   exsr      opeini
005200010528     C                   exsr      rwvao
005300110701     C*
005400110701     C* Esegue lancio TISI95R solo x chiusura
005500110701     C                   CLEAR                   TISI95DS
005600110701     C                   EVAL      I95TLA = 'C'
005700110701     C                   CALL      'TISI95R'
005800110701     C                   PARM                    TISI95DS
005900010528     C*
006000010528     C                   seton                                        lr
006100010528
006200010528
006300910830     C*--------------------------------------------------------
006400020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
006500910830     C*--------------------------------------------------------
006600010122     C     RWVAO         BEGSR
006700010528     C*
006800990914     C                   if        not %open(tivin00r)
006900990908     C                   open      tivin00r
007000990914     C                   endif
007100010122     C                   if        not %open(fnvaowwr)
007200010122     C                   open      fnvaowwr
007300990914     C                   endif
007400990910     C*
007500990910     C                   clear                   §CTROK
007600990910     C                   clear                   §CTRMO
007700990910     C                   clear                   §CTRNO
007800990910     C*
007900921023     C                   DO        *HIVAL
008000990913     C*
008100990915     C                   READ      tivin00r                               70
008200010731     C*
008300010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
008400010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
008500010618     C*
008600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
008700010618     C                   if        vinflg = '1'
008800010618     C                   eval      flgOk = '1'
008900010618     C                   endif
009000010618     C*
009100000905     C                   if        vindta > *blanks
009200000613     C                   add       1             rrnum
009300990913     C*
009400010711     C                   if        *in70 = *off and
009500010711     C                             (vinflg = *blanks
009600010711     C                              or vinflg = '0'
009700010711     C                              or vinflg = '2')
009800160921     C*
009900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010000010711     C                   if        vinflg = *blanks or vinflg = '0'
010100010711     C                   clear                   vinmsg
010200010711     C                   endif
010300020507     C*
010400160915     C* Verifico rottura di codice ORM
010500160921     C                   if        depORM = *blanks                             => 1° giro
010600160921     C                   eval      depORM = %trim(%subst(vindta:866:20))        => salvo ORM x ROTT.
010700160921     C* Eseguo importazione e traduzione dati
010800160921     C                   exsr      inzvar
010900160921     C                   exsr      defcam
011000160921     C                   exsr      impvao
011100160921     C*
011200160921     C                   else
011300160921     C                   if        depORM <> %trim(%subst(vindta:866:20))       => rottura di ORM
011400160921     C                   eval      depORM = %trim(%subst(vindta:866:20))        => salvo ORM x ROTT.
011500160921     C* Scarico buffer ORM
011600160921     C                   exsr      wriORM
011700160921     C* Eseguo importazione e traduzione dati
011800160921     C                   exsr      inzvar
011900160921     C                   exsr      defcam
012000160921     C                   exsr      impvao
012100160921     C*
012200160921     C                   else
012300160921     C* Eseguo importazione e traduzione dati
012400160921     C                   exsr      impvao
012500160921     C*
012600160921     C                   endif
012700160921     C*
012800160921     C                   endif                                                  << ROTTURA DI CODICE
012900160915     C*
013000010604     C                   if        *in31 = *off and
013100010604     C                             *in32 = *off
013200990910     C                   eval      vinflg = '1'
013300990910     C                   else
013400990910     C                   eval      vinflg = '2'
013500991022     C                   endif
013600010604     C*
013700010604     C                   endif
013800000905     C*
013900000905     C                   else
014000000905     C                   eval      vinflg = '1'
014100000905     C                   endif
014200000905     C*
014300000905     C  N70              update    tivin000
014400991022     C*
014500991022     C  N70              ENDdo
014600160921     C*
014700160921     C* Fuori ciclo scarico buffer ORM "in sospeso"
014800160921     C                   exsr      wriORM
014900010528     C*
015000010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015100161006     C                   if        cntNonEl = *zeros
015200010528     C* Se non ci sono record con errori ...
015300010528     C                   if        §ctrno = 0 and
015400161006     C                             §ctrmo = 0
015500990910     C* ... restituisco esito OK.
015600990921     C                   eval      wrkesito = '0'
015700990910     C                   else
015800000710     C                   if        §ctrok > 0
015900990921     C                   eval      wrkesito = '1'
016000000710     C                   else
016100010615     C                   if        flgOk = '0'
016200010615     C                   eval      wrkesito = '2'
016300010615     C                   else
016400010615     C                   eval      wrkesito = '6'
016500010615     C                   endif
016600990910     C                   endif
016700010528     C                   endif
016800010529     C                   else
016900010529     C                   eval      wrkesito = '9'
017000000710     C                   endif
017100990910     C*
017200990914     C                   if        %open(tivin00r)
017300990908     C                   close     tivin00r
017400990914     C                   endif
017500010122     C                   if        %open(fnvaowwr)
017600010122     C                   close     fnvaowwr
017700990914     C                   endif
017800990910     C*
017900010528     C                   if        vlrpoi <> 999
018000010528     C                   eval      invfil = vlrpoi
018100161006     C                   else
018200161006     C                   eval      invfil = vaoPOE
018300010528     C                   endif
018400010528     C*
018500990920     C                   if        §ctrok > 0
018600010528     C                             and invfil > *zeros
018700000613     C                   exsr      invio
018800990920     C                   endif
018900010612     C*
019000910830     C                   ENDSR
019100000613     C***
019200990920
019300020507
019400020507     C*----------------------------------------------------*
019500020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019600020507     C*----------------------------------------------------*
019700020507     C     INZVAR        BEGSR
019800020507     C*
019900020507     C                   Z-ADD     *zeros        Num5_0            5 0
020000020507     C                   Z-ADD     *zeros        Num3_0            3 0
020100020507     C*
020200020507     C                   ENDSR
020300020507     C*----------------------------------------------------*
020400020507     C*  IMPOSTAZIONE CAMPI COSTANTI
020500020507     C*----------------------------------------------------*
020600020507     C     DEFCAM        BEGSR
020700020507     C*
020800020507     C* Imposto i valori di default...
020900160711     C                   EVAL      VAOCOR = 0240227000
021000160711     C                   EVAL      VAOPOE = 024
021100160711     C                   EVAL      VAOKSC = 0240227
021200130503     C                   EVAL      VAOTOR = 'S'
021300130503     C                   EVAL      VAOTCO = 'F'
021400160711     C                   EVAL      VAOPAG = 'O'
021500160711     C                   EVAL      VAOCTR = '100'
021600160711     C                   EVAL      VAODDT = 'S'
021700160711     C                   EVAL      VAOORR = 1400
021800160920     C                   EVAL      ORIIDC = '00240227'
021900020507     C* ... e poi verifico se sono stati passati come parametri
022000020507     C                   IF        vlrppt > *blanks
022100020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
022200020507     C                   EXSR      CHKNUM
022300020507     C                   IF        PiInt=*on
022400020507     C                   Z-ADD     PiVal         VAOCOR
022500020507     C                   Z-ADD     PiVal         VAOCRC
022600110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
022700020507     C                   ENDIF
022800020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022900020507     C                   EXSR      CHKNUM
023000020507     C                   IF        PiInt=*on
023100020507     C                   Z-ADD     PiVal         VAOPOE
023200020507     C                   Z-ADD     PiVal         VAOPOC
023300020507     C                   ENDIF
023400020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
023500020507     C                   EXSR      CHKNUM
023600020507     C                   IF        PiInt=*on
023700020507     C                   Z-ADD     PiVal         Num3_0
023800020507     C                   MOVEL     NUM3_0        VAOCTR
023900020507     C                   ENDIF
024000160921     C*
024100130626     C* se il 17° char dei parametri è T=Test
024200130626     C                   SETOFF                                       55
024300130626     C                   IF        %subst(vlrppt:17:1) = 'T'
024400130626     C* inibisco la scrittura dei file TIORI e TIVGD
024500130626     C                   SETON                                        55
024600130626     C* imposto il PO di emissione come filiale fittizia
024700130626     C                   EVAL      VAOPOE = 999
024800130626     C                   ENDIF
024900160921     C*
025000020507     C                   ENDIF
025100020507     C*
025200020507     C                   ENDSR
025300020507     C*----------------------------------------------------*
025400020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
025500020507     C*----------------------------------------------------*
025600020507     C     IMPVAO        BEGSR
025700020507     C*
025800020507     C                   SETOFF                                       3132
025900020507     C*
026000020507     C* Reperimento campi ALFA
026100160711     C*
026200160711     C                   EVAL      VAORFA=%trim(%subst(vindta:46:25))
026300160711     C                   EVAL      VAORSR=%trim(%subst(vindta:100:40))
026400020507     C* == verifico se esiste il carattere @e, se c'è diventa A
026500020507     C     '@':'A'       XLATE     VAORSR        VAORSR
026600020507     C* ==
026700160711     C                   EVAL      VAORER=%trim(%subst(vindta:140:20))
026800160711     C                   EVAL      VAOINR=%trim(%subst(vindta:160:40))
026900160711     C                   EVAL      VAOTER=%trim(%subst(vindta:200:15))
027000160711     C                   EVAL      VAOCAR=%trim(%subst(vindta:215:9))
027100160711     C                   EVAL      VAOLOR=%trim(%subst(vindta:224:30))
027200160711     C                   EVAL      VAOPRR=%trim(%subst(vindta:254:2))
027300110701     C* Reperisco la provincia dal CAP e dalla località
027400110701     C                   IF        VAOCAR <> *blanks AND
027500110701     C                             VAOPRR  = *blanks AND
027600110701     C                             VAONAR  = *blanks
027700110701     C                   CLEAR                   TISI95DS
027800110701     C                   EVAL      I95TCN = '3'
027900110701     C                   Z-ADD     datcor        I95DAT
028000110701     C                   EVAL      I95CAP = VAOCAR
028100110701     C                   EVAL      I95LOC = VAOLOR
028200110701     C                   CALL      'TISI95R'
028300110701     C                   PARM                    TISI95DS
028400110701     C                   EVAL      VAOPRR = O95PRV
028500110701     C                   ENDIF
028600110502     C*
028700160711     C                   EVAL      VAORSC=%trim(%subst(vindta:276:40))
028800110502     C* == verifico se esiste il carattere @e, se c'è diventa A
028900110502     C     '@':'A'       XLATE     VAORSC        VAORSC
029000110502     C* ==
029100160711     C                   EVAL      VAOINC=%trim(%subst(vindta:336:40))
029200160711     C                   EVAL      VAOCAC=%trim(%subst(vindta:391:9))
029300160711     C                   EVAL      VAOLOC=%trim(%subst(vindta:400:30))
029400160711     C                   EVAL      VAOPRC=%trim(%subst(vindta:430:2))
029500160822     C                   EVAL      VAOFFD=%trim(%subst(vindta:836:1))
029600160711     C                   EVAL      VAONAM=%trim(%subst(vindta:581:30))
029700160711     C                   EVAL      VAONAR=%trim(%subst(vindta:612:3))
029800160711     C                   EVAL      VAONAC=%trim(%subst(vindta:615:3))
029900160822     C*
030000160822     C                   IF        VAONAR = 'ITA' or
030100160822     C                             VAONAR = 'IT'
030200160822     C                   EVAL      VAONAR = *blanks
030300160822     C                   ENDIF
030400160822     C*
030500160822     C                   IF        VAONAC = 'ITA' or
030600160822     C                             VAONAC = 'IT'
030700160822     C                   EVAL      VAONAC = *blanks
030800160822     C                   ENDIF
030900020507     C*
031000020507     C* Reperimento campi NUMERICI
031100020507     C*
031200020527     C* Data e ora apertura chiamata
031300020527     C                   Z-ADD     datcor        VAODAO
031400071016     C                   MOVEL     wn14          VAOOAO
031500160711     C*
031600160711     C* Codice cliente pagante e ordinante
031700160711     C                   EVAL      PiStr=%subst(vindta:1:12)
031800160711     C                   EXSR      CHKNUM
031900160711     C                   IF        PiInt=*on
032000160712     C***                EVAL      VAOCOR = PiVal
032100160711     C                   ELSE
032200160711     C                   SETON                                        32
032300160711     C                   EVAL      VAOCOR = *zeros
032400160711     C                   EVAL      vinmsg = %trimr(vinmsg)
032500160711     C                             + ' ' + 'VAOCOR'
032600160711     C                   ENDIF
032700160711     C* DAO
032800160711     C* se sbagliato ciò che è scritto su file, uso la data odierna già impostata
032900160711     C                   IF        %trim(%subst(vindta:71:8)) <> *blank  and
033000160711     C                             %trim(%subst(vindta:71:8)) <> *zero
033100160711     C                   EVAL      PiStr=%trim(%subst(vindta:71:8))
033200160711     C                   EXSR      CHKNUM
033300160711     C                   IF        PiInt=*on
033400160711     C                   EVAL      VAODAO = PiVal
033500160711     C                   ELSE
033600160711     C                   SETON                                        32
033700160711     C                   EVAL      VAODAO = *zeros
033800160711     C                   EVAL      vinmsg = %trimr(vinmsg)
033900160711     C                             + ' ' + 'VAODAO'
034000160711     C                   ENDIF
034100160711     C                   ENDIF
034200071012     C*
034300160711     C* Peso Kg (espresso in g.)
034400160711     C                   EVAL      PiStr=%subst(vindta:435:7)
034500071012     C                   EXSR      CHKNUM
034600071012     C                   IF        PiNum=*on
034700160921     C                   EVAL(H)   PiVal = PiVal/1000
034800160921     C                   ADD       PiVal         VAOPKG
034900071012     C                   ELSE
035000071012     C                   SETON                                        32
035100071012     C                   EVAL      VAOPKG = *zeros
035200071012     C                   EVAL      vinmsg = %trimr(vinmsg)
035300071012     C                             + ' ' + 'VAOPKG'
035400071012     C                   ENDIF
035500160711     C*
035600160711     C* Volume M3 (espresso in cm3) - 1 M3 = 1.000.000 cm3
035700160711     C                   IF        %subst(vindta:812:8) <> *blank
035800160711     C                   EVAL      PiStr=%subst(vindta:812:8)
035900160711     C                   EXSR      CHKNUM
036000160711     C                   IF        PiNum=*on
036100160921     C                   EVAL(H)   PiVal = PiVal/1000000
036200160921     C                   ADD       PiVal         VAOVLM
036300160711     C                   ELSE
036400160711     C                   SETON                                        32
036500160711     C                   EVAL      VAOVLM = *zeros
036600160711     C                   EVAL      vinmsg = %trimr(vinmsg)
036700160711     C                             + ' ' + 'VAOVLM'
036800160711     C                   ENDIF
036900160711     C                   ENDIF
037000160711     C*
037100160711     C* nr. colli
037200160711     C                   EVAL      PiStr=%subst(vindta:820:5)
037300160711     C                   EXSR      CHKNUM
037400160711     C                   IF        PiInt=*on
037500160711     C                   EVAL      VAONCL = PiVal
037600160711     C                   ELSE
037700160711     C                   SETON                                        32
037800160711     C                   EVAL      VAONCL = *zeros
037900160711     C                   EVAL      vinmsg = %trimr(vinmsg)
038000160711     C                             + ' ' + 'VAONCL'
038100160711     C                   ENDIF
038200160711     C*
038300160711     C* filiale emittente
038400160711     C                   IF        %subst(vindta:579:2) <> *blank
038500160711     C                   EVAL      PiStr=%subst(vindta:579:2)
038600160711     C                   EXSR      CHKNUM
038700160711     C                   IF        PiInt=*on
038800160711     C                   EVAL      VAOPOE = PiVal
038900160711     C                   ELSE
039000160711     C                   SETON                                        32
039100160711     C                   EVAL      VAOPOE = *zeros
039200160711     C                   EVAL      vinmsg = %trimr(vinmsg)
039300160711     C                             + ' ' + 'VAOPOE'
039400160711     C                   ENDIF
039500160711     C                   ENDIF
039600160711     C*
039700160711     C* filiale ritiro
039800161124     C                   IF        %subst(vindta:837:10) <> *blank
039900161124     C                   EVAL      PiStr=%subst(vindta:837:10)
040000161124     C                   EXSR      CHKNUM
040100161124     C                   IF        PiInt=*on
040200161124     C                   EVAL      VAOPOR = PiVal
040300161124     C                   ELSE
040400161124     C                   SETON                                        32
040500161124     C                   EVAL      VAOPOR = *zeros
040600161124     C                   EVAL      vinmsg = %trimr(vinmsg)
040700161124     C                             + ' ' + 'VAOPOR'
040800161124     C                   ENDIF
040900161124     C                   ENDIF
041000160711     C*
041100160711     C* filiale consegnae
041200161124     C***                IF        %subst(vindta:847:10) <> *blank
041300161124     C***                EVAL      PiStr=%subst(vindta:847:10)
041400161124     C***                EXSR      CHKNUM
041500161124     C***                IF        PiInt=*on
041600161124     C***                EVAL      VAOPOC = PiVal
041700161124     C***                ELSE
041800161124     C***                SETON                                        32
041900161124     C***                EVAL      VAOPOC = *zeros
042000161124     C***                EVAL      vinmsg = %trimr(vinmsg)
042100161124     C***                          + ' ' + 'VAOPOC'
042200161124     C***                ENDIF
042300161124     C***                ENDIF
042400020507     C*
042500020507     C                   ENDSR
042600020507     C*----------------------------------------------------*
042700160921
042800160921
042900160921     C*----------------------------------------------------*
043000160921     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043100160921     C*----------------------------------------------------*
043200160921     C     WRIORM        BEGSR
043300160921     C*
043400160921     C  N31              ADD       1             §CTROK            7 0
043500160921     C   32              ADD       1             §CTRMO            7 0
043600160921     C   31              ADD       1             §CTRNO            7 0
043700160921     C*
043800160921     C  N31              EXSR      CHKCOR
043900160921     C  N31              EXSR      WRIORI
044000160921     C* N31              EXSR      WRIVGD
044100160921     C  N31              WRITE     FNVAO000
044200160921     C*
044300160921     C                   clear                   FNVAO000
044400160921     C                   clear                   TIORI000
044500160921     C*
044600160921     C                   ENDSR
044700020507
044800020507
044900020507     C*----------------------------------------------------*
045000020507     C*  CONTROLLO NUMERICITA' CAMPI
045100020507     C*----------------------------------------------------*
045200020507     C     CHKNUM        BEGSR
045300020507     C*
045400020507     C                   IF        PiDecChr = *blanks
045500020507     C                   EVAL      PiDecChr = '.'
045600020507     C                   ENDIF
045700020507     C*
045800020507     C                   CALL(e)   'ISNUMERIC'
045900020507     C                   PARM                    PiStr            30
046000020507     C                   PARM                    PiDecChr          1
046100020507     C                   PARM      *ZEROS        PiVal            30 9
046200020507     C                   PARM      '0'           PiInt             1
046300020507     C                   PARM      '0'           PiNum             1
046400020507     C                   IF        %error
046500020507     C                   EVAL      PiNum=*off
046600020507     C                   ENDIF
046700020507     C*
046800020507     C                   ENDSR
046900020507     C***
047000030624
047100030624
047200030624     C*------------------------------------------------------------------------*
047300030624     C* CHKCOR - Considerazioni su VAOCOR
047400030624     C*------------------------------------------------------------------------*
047500030624     C     CHKCOR        BEGSR
047600030624     C*
047700030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
047800030624     C                   if        vaoTCO = 'F'
047900030624     C* Quindi verifico se il codice ordinante non è valorizzato
048000030625     C                   if        vaoCOR = *zeros  AND
048100030625     C                             vaoRSO = *blanks AND
048200030625     C                             vaoINO = *blanks AND
048300030625     C                             vaoCAO = *blanks AND
048400030625     C                             vaoLOO = *blanks AND
048500030625     C                             vaoPRO = *blanks AND
048600030625     C                             vaoNAO = *blanks
048700030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
048800030624     C                   movel     '0'           wFlgAcr           1
048900030624     C                   move      vlrKSC        wCli              7 0
049000030624     C                   move(p)   wCli          acrCRO
049100030624     C     acrCRO        setll     fnacr01l
049200030624     C                   if        %found(fnacr01l)
049300030624     C                   read      fnacr01l
049400030624     C                   dow       not %eof(fnacr01l)
049500030624     C                   movel     acrCRO        wCliAcr           7 0
049600030624     C                   if        wCliAcr = wCli
049700030624     C                   eval      vaoCOR = acrCRO
049800030624     C                   movel     '1'           wFlgAcr
049900030624     C                   leave
050000030624     C                   else
050100030624     C                   leave
050200030624     C                   endif
050300030624     C                   read      fnacr01l
050400030624     C                   enddo
050500030624     C                   endif
050600030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
050700030624     C* dati anagrafici dell'ordinante dal piano dei conti
050800030624     C                   if        wFlgAcr = '0'
050900030624     C                   clear                   BS69DS
051000030624     C                   clear                   ACODS
051100030624     C                   clear                   INDDS
051200030624     C                   clear                   CLPDS
051300030624     C                   clear                   CLSDS
051400030624     C                   move(P)   vlrKSC        I69KAC
051500030624     C                   move(P)   vlrKSC        I69KIN
051600030624     C                   call      'TIBS69R'
051700030624     C                   parm                    BS69DS
051800030624     C                   parm                    ACODS
051900030624     C                   parm                    INDDS
052000030624     C                   parm                    CLPDS
052100030624     C                   parm                    CLSDS
052200030624     C     O69ERR        ifne      '1'
052300030624     C                   eval      vaoRSO = ACORAG
052400030624     C                   eval      vaoINO = INDVIA
052500030624     C                   movel(P)  INDCAP        vaoCAO
052600030624     C                   eval      vaoLOO = INDCIT
052700030624     C                   eval      vaoPRO = INDPRV
052800030624     C                   eval      vaoNAO = INDSTA
052900030624     C                   endif
053000030624     C                   endif
053100030624     C                   endif
053200030624     C                   endif
053300030624     C*
053400030624     C                   ENDSR
053500020507
053600020507
053700010528
053800011105     C*------------------------------------------------------------------------*
053900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
054000011105     C*------------------------------------------------------------------------*
054100011105     C     WRIORI        BEGSR
054200011105     C*
054300090617     C* Come prima cosa stacco un numeratore da AZNUM
054400090617     C                   clear                   trul33ds
054500090617     C                   eval      I33OPE = *zeros
054600090617     C                   eval      I33CNU = 600
054700090617     C                   eval      I33NUM = 1
054800090617     C                   movel     TRUL33DS      KPJBU
054900090617     C                   call      'TRUL33R'
055000090617     C                   parm                    KPJBA
055100090617     C                   movel     KPJBU         TRUL33DS
055200090617     C                   if        O33ERR = *zeros
055300090617     C                   z-add     O33NRF        §numori
055400090618     c                   else
055500090618     c                   eval      §numori = *all'9'
055600090617     C                   endif
055700011105     C*
055800110502     C* Quindi imposto i campi "extra-VAO"
055900011105     C                   eval      oriPRG = §numori
056000011105     C                   eval      oriDER = datcor
056100110502     C                   eval      oriOER = oracor
056200011105     C                   eval      oriPOE = vaoPOE
056300011105     C                   eval      oriNSR = vaoNSR
056400011105     C                   eval      oriNOR = vaoNOR
056500011105     C                   eval      oriNRV = vaoNRV
056600011105     C                   eval      oriFLO = *blanks
056700011105     C*
056800011122     C* Infine valorizzo la chiave esterna sul file FNVAO
056900011105     C                   eval      dorm01 = vaoFLO
057000011105     C                   movel     §numori       §ormpg
057100160711     C* lascio vuoti i flag
057200160711     C*                  movel     'S'           §ormfr
057300170510     C                   movel     'S'           §ormks
057400160711     C*                  movel     'S'           §ormfd
057500160711     C*                  movel     'N'           §orcomc
057600011105     C                   eval      vaoFLO = dorm01
057700030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
057800030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
057900160805     C                   eval      %subst(VAOFLO:7:1) = *blanks
058000160930     C***                eval      %subst(VAOFLO:9:20-9+1) = *blanks
058100011105     C*
058200011105     C* ...quindi scrivo il file TIORI00F
058300130626     C  N55              WRITE     TIORI000
058400011105     C*
058500011105     C                   ENDSR
058600020507     C***
058700050826
058800050826
058900050826
059000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
059100050826     C     wriVGD        BEGSR
059200050826     C*
059300050826     C* Reperisco la descrizione della fase da tabella
059400050826     C                   movel(p)  'FAR'         KteCOD
059500050826     C                   movel(p)  '000'         KteKE1
059600050826     C     KEYtbe        chain     tntbe01l
059700050826     C                   if        %found(tntbe01l)
059800050826     C                   movel(P)  tbeuni        DFAR
059900050826     C                   else
060000050826     C                   clear                   DFAR
060100050826     C                   endif
060200050826     C*
060300050826     C* Valorizzo buffer record
060400050826     C                   clear                   FNVAPDS
060500050826     C                   eval      vapIDC = oriIDC
060600050826     C                   eval      vapPOE = oriPOE
060700050826     C                   eval      vapRFA = vaoRFA
060800050826     C                   eval      vapPOG = oriPOE
060900050826     C                   eval      vapDAE = oriDER
061000050826     C                   eval      vapORE = oriOER
061100050826     C                   if        d§fardva <> *blanks
061200050826     C                   eval      vapDFA = d§fardva
061300050826     C                   else
061400050826     C                   eval      vapDFA = d§fardes
061500050826     C                   endif
061600050826     C*
061700050826     C                   clear                   tivgd000
061800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
061900050826     C                   eval      vgdTIP = 'VP'
062000050826     C                   eval      vgdKSU = vapIDC
062100050826     C                   eval      vgdTSC = 'WW'
062200050826     C                   eval      vgdDAT = datcor
062300151207     C                   eval      vgdPGM = procname
062400130626     C  N55              write     tivgd000
062500050826     C*
062600050826     C                   ENDSR
062700050826     C*------------------------------------------------------------------------*
062800010528
062900010528
063000010528
063100990920      /TITLE Invio dei dati al punto operativo.
063200000613     C     invio         BEGSR
063300990920     C*
063400990920     C                   reset                   dscmz
063500010528     C                   move      invfil        cmzdst
063600990920     C                   eval      cmzfld = vlrfou
063700990920     C                   eval      cmzmbd = vlrhdl
063800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063900000710     C                   if        prmfir = *blanks
064000010122     C                   eval      cmzfla = 'FNVAO00F'
064100010122     C                   eval      cmzmba = 'FNVAO00F'
064200000710     C                   else
064300000710     C                   eval      cmzfla = prmfir
064400000710     C                   eval      cmzmba = prmfir
064500000710     C                   endif
064600990920     C                   eval      cmznrr = *zeros
064700990920     C                   move      §ctrok        cmznrr
064800010123     C                   eval      cmzlba = vlrfl1
064900990920     C                   call(e)   'TIS711C'
065000990920     C                   parm                    dscmz
065100990921     C                   parm      *blanks       esito
065200990920     C                   if        %error
065300990920     C                             or cmzerr = '1'
065400990921     C                             or esito  = '1'
065500000710     C                   eval      wrkesito = '3'
065600990920     C                   endif
065700990920     C*
065800000613     C                   ENDSR
065900000613     C***
066000990910
066100010528
066200010528
066300010528
066400010528      /TITLE Invio dei dati al punto operativo.
066500010528     C     opeini        BEGSR
066600010528     C*
066700010528     C* Inizializzo flag e contatori operativi
066800010615     C                   movel     '0'           flgOk             1
066900010528     C                   z-add     *zeros        cntNonEl         10 0
067000010528     C                   z-add     *zeros        invfil            3 0
067100010528     C*
067200010528     C                   ENDSR
067300010528     C***
067400010528
067500010528
067600010528
067700010528
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500011105     C*
068600011105     C* Campi riferito al database
068700011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
068800011105     C     *like         define    tbeke1        kteKE1
068900011105     C*
069000011105     C* Definizione chiavi di lettura
069100011105     C* TNTBE01L - parziale
069200011105     C     keytbe        klist
069300011105     C                   kfld                    kteCOD                         *UTENTE
069400011105     C                   kfld                    kteKE1                         *CODICE
069500011105     C*
069600011105     C* Reperimento data e ora corrente
069700110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
069800110502     C                   movel     wn14          oracor            6 0          *ORA (6)
069900110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070000110502     C                   eval      datcor = %dec(%date() : *ISO)
070100010529     C*
070200000613     C                   ENDSR
