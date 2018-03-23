000100130503      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
000400030624     Ffnacr01l  if   e           k disk
000500090617     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700010122     FFNVAOwwr  O    E             DISK    usropn
000800011105     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000130912     FPRTEMAIL  O    F  198        PRINTER OFLIND(*INOF) USROPN
001100990908
001200030624     D*------------------
001300030624     D* DS REPERIMENTO DATI CLIENTE
001400030624     D*-------------------
001500030624     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001600030624     D ACODS         E DS                  EXTNAME(CNACO00F)
001700030624     D INDDS         E DS                  EXTNAME(CNIND00F)
001800030624     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001900030624     D CLSDS         E DS                  EXTNAME(FNCLS00F)
002000000512     D*------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400010122     D*
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600050826     D fnvapds       e ds                  extname(fnvap00f)
002700071016     D tisi95ds      e ds
002800011105     D dorm01        e ds
002900011119     D dfar          e ds
003000110502     D trul33ds      e ds
003100110502     D kpjba         e ds
003200990910     D esito           s              1
003300000724     D prmlit          s             10
003400000710     D prmfir          s             10
003500990921     D wrkesito        s                   like(esito)
003600000613     D rrnum           s              6  0 INZ(*zeros)
003700110502     D §numori         s              7  0 inz
003800130912     D*------------------
003900130912     D* DS ridefinizione dati utente estesi spool x mailing
004000130912     D*------------------
004100130912     D TRTCM1DS      E DS
004200011122     D*-------------------
004300010731     D* COSTANTI
004400011122     D*-------------------
004500010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004600010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004700990908
004800010528
004900010528
005000010528
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400000724     C*
005500010528     C                   exsr      opeini
005600010528     C                   exsr      rwvao
005700110701     C*
005800110701     C* Esegue lancio TISI95R solo x chiusura
005900110701     C                   CLEAR                   TISI95DS
006000110701     C                   EVAL      I95TLA = 'C'
006100110701     C                   CALL      'TISI95R'
006200110701     C                   PARM                    TISI95DS
006300010528     C*
006400010528     C                   seton                                        lr
006500010528
006600010528
006700010528
006800010528
006900010528     C*--------------------------------------------------------
007000010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010528     C*--------------------------------------------------------
007200010528     C     PREELA        BEGSR
007300010528     C*
007400000724     C* SE OCCORRE SPEDIRE IN FILIALE
007500010528     C                   if        invfil <> *zeros and
007600010528     C                             flgGiro = '0'
007700010528     C*
007800010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010528     C                   eval      flgGiro = '1'
008000010529     C*
008100010529     C                   endif
008200010528     C*
008300010528     C                   ENDSR
008400010528     C***
008500010528
008600010528
008700010528     C*--------------------------------------------------------
008800010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008900010528     C*--------------------------------------------------------
009000010528     C     ENDELA        BEGSR
009100000613     C*
009200000613     C*
009300010528     C                   ENDSR
009400010528
009500010528
009600910830     C*--------------------------------------------------------
009700020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
009800910830     C*--------------------------------------------------------
009900010122     C     RWVAO         BEGSR
010000010528     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400010122     C                   if        not %open(fnvaowwr)
010500010122     C                   open      fnvaowwr
010600990914     C                   endif
010700990910     C*
010800990910     C                   clear                   §CTROK
010900990910     C                   clear                   §CTRMO
011000990910     C                   clear                   §CTRNO
011100990910     C*
011200921023     C                   DO        *HIVAL
011300990913     C*
011400990915     C                   READ      tivin00r                               70
011500010731     C*
011600010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011700010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
011800010618     C*
011900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012000010618     C                   if        vinflg = '1'
012100010618     C                   eval      flgOk = '1'
012200010618     C                   endif
012300010618     C*
012400000905     C                   if        vindta > *blanks
012500000613     C                   add       1             rrnum
012600990913     C*
012700010711     C                   if        *in70 = *off and
012800010711     C                             (vinflg = *blanks
012900010711     C                              or vinflg = '0'
013000010711     C                              or vinflg = '2')
013100990913     C*
013200010528     C                   clear                   FNVAO000
013300011105     C                   clear                   TIORI000
013400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013500010711     C                   if        vinflg = *blanks or vinflg = '0'
013600010711     C                   clear                   vinmsg
013700010711     C                   endif
013800020507     C*
013900020507     C                   exsr      inzvar
014000020527     C                   exsr      defcam
014100020507     C                   exsr      impvao
014200010122     C*
014300010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400010528     C                   eval      depfil = VAOPOE
014500010528     C                   exsr      repfil
014600020507     C                   if        depfil = invfil
014700010528     C*
014800010528     C                   exsr      PREELA
014900010710     C*
015000930409     C*
015100010604     C  N31              ADD       1             §CTROK            7 0
015200010604     C   32              ADD       1             §CTRMO            7 0
015300010604     C   31              ADD       1             §CTRNO            7 0
015400020507     C*
015500030624     C  N31              EXSR      CHKCOR
015600011105     C  N31              EXSR      WRIORI
015700110503     C* N31              EXSR      WRIVGD
015800010122     C  N31              WRITE     FNVAO000
015900130912     C  N31              EXSR      INVIOEML
016000990910     C*
016100010604     C                   if        *in31 = *off and
016200010604     C                             *in32 = *off
016300990910     C                   eval      vinflg = '1'
016400990910     C                   else
016500990910     C                   eval      vinflg = '2'
016600991022     C                   endif
016700990910     C                   endif
016800010604     C*
016900010604     C                   endif
017000000905     C*
017100000905     C                   else
017200000905     C                   eval      vinflg = '1'
017300000905     C                   endif
017400000905     C*
017500000905     C  N70              update    tivin000
017600991022     C*
017700991022     C  N70              ENDdo
017800010528     C*
017900010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018000010528     C                   if        cntNonEl = *zeros or
018100010528     C                             flgMulti = '0'
018200010528     C* Se non ci sono record con errori ...
018300010528     C                   if        §ctrno = 0 and
018400010604     C                             §ctrmo = 0 and
018500010528     C                             flgStato <> '2'
018600990910     C* ... restituisco esito OK.
018700990921     C                   eval      wrkesito = '0'
018800990910     C                   else
018900000710     C                   if        §ctrok > 0
019000990921     C                   eval      wrkesito = '1'
019100000710     C                   else
019200010615     C                   if        flgOk = '0'
019300010615     C                   eval      wrkesito = '2'
019400010615     C                   else
019500010615     C                   eval      wrkesito = '6'
019600010615     C                   endif
019700990910     C                   endif
019800010528     C                   endif
019900010529     C                   else
020000010529     C                   eval      wrkesito = '9'
020100000710     C                   endif
020200990910     C*
020300990914     C                   if        %open(tivin00r)
020400990908     C                   close     tivin00r
020500990914     C                   endif
020600010122     C                   if        %open(fnvaowwr)
020700010122     C                   close     fnvaowwr
020800990914     C                   endif
020900990910     C*
021000010528     C                   if        vlrpoi <> 999
021100010528     C                   eval      invfil = vlrpoi
021200010528     C                   endif
021300010528     C*
021400990920     C                   if        §ctrok > 0
021500010528     C                             and invfil > *zeros
021600000613     C                   exsr      invio
021700990920     C                   endif
021800010612     C*
021900010612     C                   if        flgGiro = '1'
022000010612     C                   exsr      endela
022100010612     C                   endif
022200010528     C*
022300910830     C                   ENDSR
022400000613     C***
022500990920
022600020507
022700020507     C*----------------------------------------------------*
022800020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022900020507     C*----------------------------------------------------*
023000020507     C     INZVAR        BEGSR
023100020507     C*
023200020507     C                   Z-ADD     *zeros        Num5_0            5 0
023300020507     C                   Z-ADD     *zeros        Num3_0            3 0
023400020507     C*
023500020507     C                   ENDSR
023600020507     C*----------------------------------------------------*
023700020507     C*  IMPOSTAZIONE CAMPI COSTANTI
023800020507     C*----------------------------------------------------*
023900020507     C     DEFCAM        BEGSR
024000020507     C*
024100020507     C* Imposto i valori di default...
024200130626     C                   EVAL      VAOPOE = 210
024300130503     C                   EVAL      VAOTOR = 'S'
024400130503     C                   EVAL      VAOTCO = 'F'
024500130626     C                   EVAL      VAONAR = *blank
024600130626     C                   EVAL      VAOORR = 1400
024700130626     C                   EVAL      VAORMP = 'P'
024800130626     C                   EVAL      VAOPAG = 'O'
024900130626     C                   EVAL      VAONAC = *blank
025000020507     C* ... e poi verifico se sono stati passati come parametri
025100020507     C                   IF        vlrppt > *blanks
025200020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
025300020507     C                   EXSR      CHKNUM
025400020507     C                   IF        PiInt=*on
025500020507     C                   Z-ADD     PiVal         VAOCOR
025600020507     C                   Z-ADD     PiVal         VAOCRC
025700110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
025800020507     C                   ENDIF
025900020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026000020507     C                   EXSR      CHKNUM
026100020507     C                   IF        PiInt=*on
026200020507     C                   Z-ADD     PiVal         VAOPOE
026300020507     C                   Z-ADD     PiVal         VAOPOC
026400020507     C                   ENDIF
026500020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026600020507     C                   EXSR      CHKNUM
026700020507     C                   IF        PiInt=*on
026800020507     C                   Z-ADD     PiVal         Num3_0
026900020507     C                   MOVEL     NUM3_0        VAOCTR
027000020507     C                   ENDIF
027100130626
027200130626     C* se il 17° char dei parametri è T=Test
027300130626     C                   SETOFF                                       55
027400130626     C                   IF        %subst(vlrppt:17:1) = 'T'
027500130626     C* inibisco la scrittura dei file TIORI e TIVGD
027600130626     C                   SETON                                        55
027700130626     C* imposto il PO di emissione come filiale fittizia
027800130626     C                   EVAL      VAOPOE = 999
027900130626     C                   ENDIF
028000130626
028100020507     C                   ENDIF
028200020507     C*
028300020507     C                   ENDSR
028400020507     C*----------------------------------------------------*
028500020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
028600020507     C*----------------------------------------------------*
028700020507     C     IMPVAO        BEGSR
028800020507     C*
028900020507     C                   SETOFF                                       3132
029000020507     C*
029100020507     C* Reperimento campi ALFA
029200020507     C*
029300110701     C* Dati x il ritiro
029400110803     C                   EVAL      VAORSR=%trim(%subst(vindta:219:40))
029500020507     C* == verifico se esiste il carattere @e, se c'è diventa A
029600020507     C     '@':'A'       XLATE     VAORSR        VAORSR
029700020507     C* ==
029800110803     C                   EVAL      VAOINR=%trim(%subst(vindta:259:35))
029900110803     C                   EVAL      VAOCAR=%trim(%subst(vindta:294:5))
030000110803     C                   EVAL      VAOLOR=%trim(%subst(vindta:299:40))
030100110803     C                   EVAL      VAOPRR=%trim(%subst(vindta:339:2))
030200110803     C                   EVAL      VAOTER=%trim(%subst(vindta:341:15))
030300111223     C                   EVAL      VAORER=%trim(%subst(vindta:356:20))
030400110701     C* Reperisco la provincia dal CAP e dalla località
030500110701     C                   IF        VAOCAR <> *blanks AND
030600110701     C                             VAOPRR  = *blanks AND
030700110701     C                             VAONAR  = *blanks
030800110701     C                   CLEAR                   TISI95DS
030900110701     C                   EVAL      I95TCN = '3'
031000110701     C                   Z-ADD     datcor        I95DAT
031100110701     C                   EVAL      I95CAP = VAOCAR
031200110701     C                   EVAL      I95LOC = VAOLOR
031300110701     C                   CALL      'TISI95R'
031400110701     C                   PARM                    TISI95DS
031500110701     C                   EVAL      VAOPRR = O95PRV
031600110701     C                   ENDIF
031700110502     C*
031800110701     C* Dati x la consegna
031900110803     C                   EVAL      VAORSC=%trim(%subst(vindta:506:40))
032000110502     C* == verifico se esiste il carattere @e, se c'è diventa A
032100110502     C     '@':'A'       XLATE     VAORSC        VAORSC
032200110502     C* ==
032300110803     C                   EVAL      VAOINC=%trim(%subst(vindta:546:35))
032400110803     C                   EVAL      VAOCAC=%trim(%subst(vindta:581:5))
032500110803     C                   EVAL      VAOLOC=%trim(%subst(vindta:586:40))
032600110803     C                   EVAL      VAOPRC=%trim(%subst(vindta:626:2))
032700130626     C*
032800110701     C* Dati genericio ORM
032900110803     C                   EVAL      VAORFA=%trim(%subst(vindta:745:10))
033000111223     C                   EVAL      VAONO1=%trim(%subst(vindta:426:35))
033100111223     C                   EVAL      VAONO2=%trim(%subst(vindta:426+35:35))
033200111223
033300111223      * Natura merce
033400111223     C                   SELECT
033500130626     c                   WHEN      %subst(vindta:706:1) = 'S'
033600130626     C                   EVAL      VAONAM='BUSTE'
033700130626     c                   WHEN      %subst(vindta:706:1) = 'C'
033800130626     C                   EVAL      VAONAM='COLLI'
033900130626     c                   WHEN      %subst(vindta:706:1) = 'B'
034000130626     C                   EVAL      VAONAM='BAULETTI PICCOLI'
034100130626     c                   WHEN      %subst(vindta:706:1) = 'D'
034200130626     C                   EVAL      VAONAM='BAULETTI GRANDI'
034300111223     c                   ENDSL
034400110803     C*
034500111223      * Fermo deposito
034600110803     C                   SELECT
034700130923     C                   WHEN      %subst(vindta:709:1) = '1'
034800130626     C                   EVAL      VAOFFD='S'
034900110803     C                   ENDSL
035000020507     C*
035100020507     C* Reperimento campi NUMERICI
035200020507     C*
035300020527     C* Data e ora apertura chiamata
035400020527     C                   Z-ADD     datcor        VAODAO
035500071016     C                   MOVEL     wn14          VAOOAO
035600020507     C*
035700130626     C* Codice cliente pagante e ordinante
035800130626     C                   EVAL      PiStr=%subst(vindta:1:11)
035900020507     C                   EXSR      CHKNUM
036000020507     C                   IF        PiInt=*on
036100130626     C                   Z-ADD     PiVal         VAOKSC
036200130626     C                   EVAL      VAOCOR = VAOKSC * 1000
036300020507     C                   ELSE
036400020507     C                   SETON                                        32
036500130626     C                   EVAL      VAOKSC = *zeros
036600020507     C                   EVAL      vinmsg = %trimr(vinmsg)
036700130626     C                             + ' ' + 'VAOKSC'
036800020507     C                   ENDIF
036900130626     C*
037000130626     C* Data ritiro richiesta
037100130626     C                   EVAL      PiStr=%subst(vindta:682:4)+
037200130626     C                                   %subst(vindta:680:2)+
037300130626     C                                   %subst(vindta:678:2)
037400130626     C                   EXSR      CHKNUM
037500130626     C                   IF        PiInt=*on
037600130626     C                   Z-ADD     PiVal         VAODAR
037700130626     C                   ELSE
037800130626     C                   SETON                                        32
037900130626     C                   EVAL      VAODAR = *zeros
038000130626     C                   EVAL      vinmsg = %trimr(vinmsg)
038100130626     C                             + ' ' + 'VAODAR'
038200130626     C                   ENDIF
038300130626     C*
038400130626     C* Ora ritiro richiesta
038500130626     C                   EVAL      PiStr=%subst(vindta:702:4)
038600130626     C                   EXSR      CHKNUM
038700130626     C                   IF        PiInt=*on
038800130626     C                   Z-ADD     PiVal         VAOORR
038900130626     C                   ELSE
039000130626     C                   SETON                                        32
039100130626     C                   EVAL      VAOORR = *zeros
039200130626     C                   EVAL      vinmsg = %trimr(vinmsg)
039300130626     C                             + ' ' + 'VAOORR'
039400130626     C                   ENDIF
039500071012     C*
039600071012     C* Colli
039700110803     C                   EVAL      PiStr=%subst(vindta:723:5)
039800071012     C                   EXSR      CHKNUM
039900071012     C                   IF        PiInt=*on
040000071012     C                   Z-ADD     PiVal         VAONCL
040100071012     C                   ELSE
040200071012     C                   SETON                                        32
040300071012     C                   EVAL      VAONCL = *zeros
040400071012     C                   EVAL      vinmsg = %trimr(vinmsg)
040500071012     C                             + ' ' + 'VAONCL'
040600071012     C                   ENDIF
040700071012     C*
040800071012     C* Peso Kg
040900130626     C                   EVAL      PiStr=%subst(vindta:728:8)
041000071012     C                   EXSR      CHKNUM
041100071012     C                   IF        PiNum=*on
041200130626     C                   EVAL      VAOPKG = PiVal/100
041300071012     C                   ELSE
041400071012     C                   SETON                                        32
041500071012     C                   EVAL      VAOPKG = *zeros
041600071012     C                   EVAL      vinmsg = %trimr(vinmsg)
041700071012     C                             + ' ' + 'VAOPKG'
041800071012     C                   ENDIF
041900111223
042000111223      * forzo 1 collo se = 0
042100111223     c                   IF        VAOncl = 0
042200111223     c                   eval      VAOncl = 1
042300111223     c                   ENDIF
042400111223
042500020507     C*
042600020507     C                   ENDSR
042700020507     C*----------------------------------------------------*
042800020507
042900020507
043000020507
043100020507     C*----------------------------------------------------*
043200020507     C*  CONTROLLO NUMERICITA' CAMPI
043300020507     C*----------------------------------------------------*
043400020507     C     CHKNUM        BEGSR
043500020507     C*
043600020507     C                   IF        PiDecChr = *blanks
043700020507     C                   EVAL      PiDecChr = '.'
043800020507     C                   ENDIF
043900020507     C*
044000020507     C                   CALL(e)   'ISNUMERIC'
044100020507     C                   PARM                    PiStr            30
044200020507     C                   PARM                    PiDecChr          1
044300020507     C                   PARM      *ZEROS        PiVal            30 9
044400020507     C                   PARM      '0'           PiInt             1
044500020507     C                   PARM      '0'           PiNum             1
044600020507     C                   IF        %error
044700020507     C                   EVAL      PiNum=*off
044800020507     C                   ENDIF
044900020507     C*
045000020507     C                   ENDSR
045100020507     C***
045200030624
045300030624
045400030624     C*------------------------------------------------------------------------*
045500030624     C* CHKCOR - Considerazioni su VAOCOR
045600030624     C*------------------------------------------------------------------------*
045700030624     C     CHKCOR        BEGSR
045800030624     C*
045900030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
046000030624     C                   if        vaoTCO = 'F'
046100030624     C* Quindi verifico se il codice ordinante non è valorizzato
046200030625     C                   if        vaoCOR = *zeros  AND
046300030625     C                             vaoRSO = *blanks AND
046400030625     C                             vaoINO = *blanks AND
046500030625     C                             vaoCAO = *blanks AND
046600030625     C                             vaoLOO = *blanks AND
046700030625     C                             vaoPRO = *blanks AND
046800030625     C                             vaoNAO = *blanks
046900030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
047000030624     C                   movel     '0'           wFlgAcr           1
047100030624     C                   move      vlrKSC        wCli              7 0
047200030624     C                   move(p)   wCli          acrCRO
047300030624     C     acrCRO        setll     fnacr01l
047400030624     C                   if        %found(fnacr01l)
047500030624     C                   read      fnacr01l
047600030624     C                   dow       not %eof(fnacr01l)
047700030624     C                   movel     acrCRO        wCliAcr           7 0
047800030624     C                   if        wCliAcr = wCli
047900030624     C                   eval      vaoCOR = acrCRO
048000030624     C                   movel     '1'           wFlgAcr
048100030624     C                   leave
048200030624     C                   else
048300030624     C                   leave
048400030624     C                   endif
048500030624     C                   read      fnacr01l
048600030624     C                   enddo
048700030624     C                   endif
048800030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
048900030624     C* dati anagrafici dell'ordinante dal piano dei conti
049000030624     C                   if        wFlgAcr = '0'
049100030624     C                   clear                   BS69DS
049200030624     C                   clear                   ACODS
049300030624     C                   clear                   INDDS
049400030624     C                   clear                   CLPDS
049500030624     C                   clear                   CLSDS
049600030624     C                   move(P)   vlrKSC        I69KAC
049700030624     C                   move(P)   vlrKSC        I69KIN
049800030624     C                   call      'TIBS69R'
049900030624     C                   parm                    BS69DS
050000030624     C                   parm                    ACODS
050100030624     C                   parm                    INDDS
050200030624     C                   parm                    CLPDS
050300030624     C                   parm                    CLSDS
050400030624     C     O69ERR        ifne      '1'
050500030624     C                   eval      vaoRSO = ACORAG
050600030624     C                   eval      vaoINO = INDVIA
050700030624     C                   movel(P)  INDCAP        vaoCAO
050800030624     C                   eval      vaoLOO = INDCIT
050900030624     C                   eval      vaoPRO = INDPRV
051000030624     C                   eval      vaoNAO = INDSTA
051100030624     C                   endif
051200030624     C                   endif
051300030624     C                   endif
051400030624     C                   endif
051500030624     C*
051600030624     C                   ENDSR
051700020507
051800020507
051900010528
052000011105     C*------------------------------------------------------------------------*
052100011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
052200011105     C*------------------------------------------------------------------------*
052300011105     C     WRIORI        BEGSR
052400011105     C*
052500090617     C* Come prima cosa stacco un numeratore da AZNUM
052600090617     C                   clear                   trul33ds
052700090617     C                   eval      I33OPE = *zeros
052800090617     C                   eval      I33CNU = 600
052900090617     C                   eval      I33NUM = 1
053000090617     C                   movel     TRUL33DS      KPJBU
053100090617     C                   call      'TRUL33R'
053200090617     C                   parm                    KPJBA
053300090617     C                   movel     KPJBU         TRUL33DS
053400090617     C                   if        O33ERR = *zeros
053500090617     C                   z-add     O33NRF        §numori
053600090618     c                   else
053700090618     c                   eval      §numori = *all'9'
053800090617     C                   endif
053900011105     C*
054000110502     C* Quindi imposto i campi "extra-VAO"
054100011105     C                   eval      oriPRG = §numori
054200011105     C                   eval      oriDER = datcor
054300110502     C                   eval      oriOER = oracor
054400011105     C                   eval      oriPOE = vaoPOE
054500011105     C                   eval      oriNSR = vaoNSR
054600011105     C                   eval      oriNOR = vaoNOR
054700011105     C                   eval      oriNRV = vaoNRV
054800011105     C                   eval      oriFLO = *blanks
054900011105     C*
055000011122     C* Infine valorizzo la chiave esterna sul file FNVAO
055100011105     C                   eval      dorm01 = vaoFLO
055200011105     C                   movel     §numori       §ormpg
055300110502     C                   movel     'S'           §ormfr
055400111223     C                   movel     'S'           §ormks
055500111223     C                   movel     'S'           §ormfd
055600130626     C                   movel     'N'           §orcomc
055700011105     C                   eval      vaoFLO = dorm01
055800030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
055900030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
056000011105     C*
056100011105     C* ...quindi scrivo il file TIORI00F
056200130626     C  N55              WRITE     TIORI000
056300011105     C*
056400011105     C                   ENDSR
056500020507     C***
056600050826
056700050826
056800050826
056900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
057000050826     C     wriVGD        BEGSR
057100050826     C*
057200050826     C* Reperisco la descrizione della fase da tabella
057300050826     C                   movel(p)  'FAR'         KteCOD
057400050826     C                   movel(p)  '000'         KteKE1
057500050826     C     KEYtbe        chain     tntbe01l
057600050826     C                   if        %found(tntbe01l)
057700050826     C                   movel(P)  tbeuni        DFAR
057800050826     C                   else
057900050826     C                   clear                   DFAR
058000050826     C                   endif
058100050826     C*
058200050826     C* Valorizzo buffer record
058300050826     C                   clear                   FNVAPDS
058400050826     C                   eval      vapIDC = oriIDC
058500050826     C                   eval      vapPOE = oriPOE
058600050826     C                   eval      vapRFA = vaoRFA
058700050826     C                   eval      vapPOG = oriPOE
058800050826     C                   eval      vapDAE = oriDER
058900050826     C                   eval      vapORE = oriOER
059000050826     C                   if        d§fardva <> *blanks
059100050826     C                   eval      vapDFA = d§fardva
059200050826     C                   else
059300050826     C                   eval      vapDFA = d§fardes
059400050826     C                   endif
059500050826     C*
059600050826     C                   clear                   tivgd000
059700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
059800050826     C                   eval      vgdTIP = 'VP'
059900050826     C                   eval      vgdKSU = vapIDC
060000050826     C                   eval      vgdTSC = 'WW'
060100050826     C                   eval      vgdDAT = datcor
060200111223     C                   eval      vgdPGM = 'TITZ04R'
060300130626     C  N55              write     tivgd000
060400050826     C*
060500050826     C                   ENDSR
060600050826     C*------------------------------------------------------------------------*
060700011119
060800010528
060900010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061000010528     C     repfil        BEGSR
061100010528     C*
061200010528     C                   if        invfil = *zeros and
061300010528     C                             depfil > *zeros and
061400010613     C                             (vinflg = *blanks or
061500010613     C                              vinflg = *zeros)
061600010528     C
061700010528     C                   eval      invfil = depfil
061800010528     C                   endif
061900010528     C*
062000010529     C                   if        depfil <> invfil and
062100010529     C                             invfil > *zeros
062200010528     C                   eval      flgMulti = '1'
062300010528     C                   if        vinflg = *blanks
062400010528     C                   add       1             cntNonEl
062500010528     C                   endif
062600010528     C                   endif
062700010528     C*
062800010528     C                   if        vinflg = '2'
062900010528     C                   eval      flgStato = '2'
063000010528     C                   endif
063100010528     C*
063200010528     C                   ENDSR
063300010528     C***
063400010528
063500010528
063600010528
063700010528
063800990920      /TITLE Invio dei dati al punto operativo.
063900000613     C     invio         BEGSR
064000990920     C*
064100990920     C                   reset                   dscmz
064200010528     C                   move      invfil        cmzdst
064300990920     C                   eval      cmzfld = vlrfou
064400990920     C                   eval      cmzmbd = vlrhdl
064500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
064600000710     C                   if        prmfir = *blanks
064700010122     C                   eval      cmzfla = 'FNVAO00F'
064800010122     C                   eval      cmzmba = 'FNVAO00F'
064900000710     C                   else
065000000710     C                   eval      cmzfla = prmfir
065100000710     C                   eval      cmzmba = prmfir
065200000710     C                   endif
065300990920     C                   eval      cmznrr = *zeros
065400990920     C                   move      §ctrok        cmznrr
065500010123     C                   eval      cmzlba = vlrfl1
065600990920     C                   call(e)   'TIS711C'
065700990920     C                   parm                    dscmz
065800990921     C                   parm      *blanks       esito
065900990920     C                   if        %error
066000990920     C                             or cmzerr = '1'
066100990921     C                             or esito  = '1'
066200000710     C                   eval      wrkesito = '3'
066300990920     C                   endif
066400990920     C*
066500000613     C                   ENDSR
066600000613     C***
066700990910
066800010528
066900010528
067000010528
067100010528      /TITLE Invio dei dati al punto operativo.
067200010528     C     opeini        BEGSR
067300010528     C*
067400010528     C* Inizializzo flag e contatori operativi
067500010528     C                   movel     '0'           flgGiro           1
067600010528     C                   movel     '0'           flgMulti          1
067700010528     C                   movel     '1'           flgStato          1
067800010615     C                   movel     '0'           flgOk             1
067900010528     C                   z-add     *zeros        cntNonEl         10 0
068000010528     C                   z-add     *zeros        depfil            3 0
068100010528     C                   z-add     *zeros        invfil            3 0
068200010528     C*
068300010528     C                   ENDSR
068400010528     C***
068500130912
068600130912
068700130912
068800130912     C*----------------------------------------------------*
068900130912     C*  INVIO EMAIL
069000130912     C*----------------------------------------------------*
069100130912     C     INVIOEML      BEGSR
069200130912     C*
069300130912     C                   EXSR      PRESTA
069400130912     C                   EXSR      STATESTO
069500130912     C                   EXSR      ENDSTA
069600130912     C*
069700130912     C                   ENDSR
069800130912     C*----------------------------------------------------*
069900130912     C*  PREPARAZIONE STAMPA
070000130912     C*----------------------------------------------------*
070100130912     C     PRESTA        BEGSR
070200130912     C*
070300130912     C* Imposto quindi i dati utenti estesi
070400130912     C                   EVAL      §CM1MITT = 'vasNoReply'
070500130912     C                   EVAL      §CM1VAR  = '*OBJM*'+'Risposta automatica ' +
070600130912     C                                        'BRT: Conferma ricezione dati'
070700130912     C                   EVAL      §CM1TIPS = 'S01'
070800130912 xxx C                   EVAL      §CM1IDP  = '5'
070900130912     C                   MOVEL(P)  '046'         §CM1PO
071000130912     C                   EVAL      §CM1DST  = %trim(%subst(vindta:376:50))
071100130912     C*
071200130912     C* Eseguo override x generare uno spool ad "hoc"
071300130912     C                   CALL      'TRUL83C1'
071400130912     C                   PARM      'P'           OPZIONE           1
071500130912     C                   PARM      'PRTEMAIL'    PRTFILE          10
071600130912     C                   PARM                    TRTCM1DS
071700130912     C                   PARM      'EMAILIN_K'   OUTQ             10
071800130912     C                   PARM                    Status            1
071900130912     C*
072000130912     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
072100130912     C                   IF        Status = '2'
072200130912     C                   SETON                                        60
072300130912     C                   ELSE
072400130912     C                   OPEN      PRTEMAIL
072500130912     C                   SETOFF                                       60
072600130912     C                   ENDIF
072700130912     C*
072800130912     C                   ENDSR
072900130912
073000130912     C*----------------------------------------------------*
073100130912     C*  STAMPA RIGA DI TESTO
073200130912     C*----------------------------------------------------*
073300130912     C     STATESTO      BEGSR
073400130912     C*
073500130912     C                   movel     *blanks       wRiga           100
073600130912     C*
073700130912     C                   if        not *in60
073800130912     C                   eval      wRiga = '§§TestoCorpo§§#'
073900130912     C                   except    RIGA
074000130912     C                   eval      wRiga = 'Gentile cliente,'+'<BR><BR>'
074100130912     C                   except    RIGA
074200130912     C                   eval      wRiga = '   abbiamo ricevuto '+
074300130912     C                             'i Vs dati relativi ad una richiesta di '   +
074400130912     C                             'ritiro.'+'<BR><BR>'
074500130912     C                   except    RIGA
074600130912     C                   eval      wRiga = 'Cordiali saluti'+'<BR>'
074700130912     C                   except    RIGA
074800130912     C                   eval      wRiga = 'BRT S.p.A.'+'<BR>'
074900130912     C                   except    RIGA
075000130912     C                   endif
075100130912     C*
075200130912     C                   ENDSR
075300130912
075400130912
075500130912     C*----------------------------------------------------*
075600130912     C*  CHIUSURA STAMPA
075700130912     C*----------------------------------------------------*
075800130912     C     ENDSTA        BEGSR
075900130912     C*
076000130912     C                   CLOSE     PRTEMAIL
076100130912     C*
076200130912     C* Eseguo delete override precedente
076300130912     C                   CALL      'TRUL83C1'
076400130912     C                   PARM      'D'           OPZIONE
076500130912     C                   PARM                    PRTFILE
076600130912     C                   PARM                    TRTCM1DS
076700130912     C                   PARM                    OUTQ
076800130912     C                   PARM                    Status
076900130912     C*
077000130912     C                   ENDSR
077100010528
077200010528
077300010528
077400000613     C     *inzsr        BEGSR
077500990910     C*
077600990910     C     *entry        plist
077700990920     C                   parm                    tivlrds
077800990921     C                   parm      wrkesito      esito
077900000724     C                   parm                    prmlit
078000000710     C                   parm                    prmfir
078100011105     C*
078200011105     C* Campi riferito al database
078300011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
078400011105     C     *like         define    tbeke1        kteKE1
078500011105     C*
078600011105     C* Definizione chiavi di lettura
078700011105     C* TNTBE01L - parziale
078800011105     C     keytbe        klist
078900011105     C                   kfld                    kteCOD                         *UTENTE
079000011105     C                   kfld                    kteKE1                         *CODICE
079100011105     C*
079200011105     C* Reperimento data e ora corrente
079300110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
079400110502     C                   movel     wn14          oracor            6 0          *ORA (6)
079500110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079600110502     C                   eval      datcor = %dec(%date() : *ISO)
079700010529     C*
079800000613     C                   ENDSR
079900130912
080000130912     O*------------------------------------------------------------------------*
080100130912     O* STAMPA
080200130912     O*------------------------------------------------------------------------*
080300130912     OPRTEMAIL  E            RIGA        1
080400130912     O                       wRiga               +1
