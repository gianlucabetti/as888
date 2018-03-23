000100130503      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200141021      *
000300141021      * PARTICOLARITA':
000400141021      * nei traduttori VAO x .txt std la posiz. parametri 11x3 è sia VAOPOE che VAOPOC, in questo
000500141021      * si è differenziato: 11x3 = VAOPOE 17x3 = VAOPOC
000600141021      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800120209
000900030624     Ffnacr01l  if   e           k disk
001000090617     Ftntbe01l  if   e           k disk
001100990910     Ftivin00r  uF   E             DISK    usropn
001200010122     FFNVAOwwr  O    E             DISK    usropn
001300011105     Ftiori00f  O    E             DISK
001400050826     FTIVGD00F  O    E             DISK
001500990908
001600030624     D*------------------
001700030624     D* DS REPERIMENTO DATI CLIENTE
001800030624     D*-------------------
001900030624     D BS69DS        E DS                  EXTNAME(TIBS69DS)
002000030624     D ACODS         E DS                  EXTNAME(CNACO00F)
002100030624     D INDDS         E DS                  EXTNAME(CNIND00F)
002200030624     D CLPDS         E DS                  EXTNAME(CNCLP00F)
002300030624     D CLSDS         E DS                  EXTNAME(FNCLS00F)
002400000512     D*------------
002500990920     D dscmz         e ds                  inz
002600990910     D psds           sds
002700990910     D  procname         *PROC
002800010122     D*
002900990920     D tivlrds       e ds                  extname(tivlr00f)
003000050826     D fnvapds       e ds                  extname(fnvap00f)
003100071016     D tisi95ds      e ds
003200011105     D dorm01        e ds
003300011119     D dfar          e ds
003400110502     D trul33ds      e ds
003500110502     D kpjba         e ds
003600990910     D esito           s              1
003700000724     D prmlit          s             10
003800000710     D prmfir          s             10
003900990921     D wrkesito        s                   like(esito)
004000000613     D rrnum           s              6  0 INZ(*zeros)
004100110502     D §numori         s              7  0 inz
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
015900990910     C*
016000010604     C                   if        *in31 = *off and
016100010604     C                             *in32 = *off
016200990910     C                   eval      vinflg = '1'
016300990910     C                   else
016400990910     C                   eval      vinflg = '2'
016500991022     C                   endif
016600990910     C                   endif
016700010604     C*
016800010604     C                   endif
016900000905     C*
017000000905     C                   else
017100000905     C                   eval      vinflg = '1'
017200000905     C                   endif
017300000905     C*
017400000905     C  N70              update    tivin000
017500991022     C*
017600991022     C  N70              ENDdo
017700010528     C*
017800010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017900010528     C                   if        cntNonEl = *zeros or
018000010528     C                             flgMulti = '0'
018100010528     C* Se non ci sono record con errori ...
018200010528     C                   if        §ctrno = 0 and
018300010604     C                             §ctrmo = 0 and
018400010528     C                             flgStato <> '2'
018500990910     C* ... restituisco esito OK.
018600990921     C                   eval      wrkesito = '0'
018700990910     C                   else
018800000710     C                   if        §ctrok > 0
018900990921     C                   eval      wrkesito = '1'
019000000710     C                   else
019100010615     C                   if        flgOk = '0'
019200010615     C                   eval      wrkesito = '2'
019300010615     C                   else
019400010615     C                   eval      wrkesito = '6'
019500010615     C                   endif
019600990910     C                   endif
019700010528     C                   endif
019800010529     C                   else
019900010529     C                   eval      wrkesito = '9'
020000000710     C                   endif
020100990910     C*
020200990914     C                   if        %open(tivin00r)
020300990908     C                   close     tivin00r
020400990914     C                   endif
020500010122     C                   if        %open(fnvaowwr)
020600010122     C                   close     fnvaowwr
020700990914     C                   endif
020800990910     C*
020900010528     C                   if        vlrpoi <> 999
021000010528     C                   eval      invfil = vlrpoi
021100010528     C                   endif
021200010528     C*
021300990920     C                   if        §ctrok > 0
021400010528     C                             and invfil > *zeros
021500000613     C                   exsr      invio
021600990920     C                   endif
021700010612     C*
021800010612     C                   if        flgGiro = '1'
021900010612     C                   exsr      endela
022000010612     C                   endif
022100010528     C*
022200910830     C                   ENDSR
022300000613     C***
022400990920
022500020507
022600020507     C*----------------------------------------------------*
022700020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800020507     C*----------------------------------------------------*
022900020507     C     INZVAR        BEGSR
023000020507     C*
023100020507     C                   Z-ADD     *zeros        Num5_0            5 0
023200020507     C                   Z-ADD     *zeros        Num3_0            3 0
023300020507     C*
023400020507     C                   ENDSR
023500020507     C*----------------------------------------------------*
023600020507     C*  IMPOSTAZIONE CAMPI COSTANTI
023700020507     C*----------------------------------------------------*
023800020507     C     DEFCAM        BEGSR
023900020507     C*
024000020507     C* Imposto i valori di default...
024100130503     C                   EVAL      VAOPOE = 099
024200130503     C                   EVAL      VAOTOR = 'S'
024300130503     C                   EVAL      VAOTCO = 'F'
024400150519     C                   EVAL      VAOCOR = 0992276000
024500130503     C                   EVAL      VAOPAG = 'O'
024600150519     C                   EVAL      VAOKSC = 0992276
024700130503     C                   EVAL      VAODDT = 'N'
024800150730     C***                EVAL      VAOCTR = '101'
024900130503     C                   EVAL      VAOORR = 1000
025000150519     C                   EVAL      ORIIDC = '00992276'
025100020507     C* ... e poi verifico se sono stati passati come parametri
025200020507     C                   IF        vlrppt > *blanks
025300020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
025400020507     C                   EXSR      CHKNUM
025500020507     C                   IF        PiInt=*on
025600020507     C                   Z-ADD     PiVal         VAOCOR
025700020507     C                   Z-ADD     PiVal         VAOCRC
025800110504     C                   EVAL      ORIIDC = '0'+%subst(vlrppt:1:7)
025900020507     C                   ENDIF
026000020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026100020507     C                   EXSR      CHKNUM
026200020507     C                   IF        PiInt=*on
026300020507     C                   Z-ADD     PiVal         VAOPOE
026400020507     C                   ENDIF
026500020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026600020507     C                   EXSR      CHKNUM
026700020507     C                   IF        PiInt=*on
026800020507     C                   Z-ADD     PiVal         Num3_0
026900020507     C                   MOVEL     NUM3_0        VAOCTR
027000020507     C                   ENDIF
027100141021     C                   EVAL      PiStr=%trim(%subst(vlrppt:17:3))
027200141021     C                   EXSR      CHKNUM
027300141021     C                   IF        PiInt=*on
027400141021     C                   Z-ADD     PiVal         VAOPOC
027500141021     C                   ENDIF
027600170309     C                   EVAL      PiStr=%trim(%subst(vlrppt:20:7))
027700170131     C                   EXSR      CHKNUM
027800170131     C                   IF        PiInt=*on
027900170131     C                   Z-ADD     PiVal         VAOKSC
028000170131     C                   ENDIF
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
032700110502     C*
032800110701     C* Dati genericio ORM
032900110803     C                   EVAL      VAORFA=%trim(%subst(vindta:745:10))
033000111223     C                   EVAL      VAONO1=%trim(%subst(vindta:426:35))
033100111223     C                   EVAL      VAONO2=%trim(%subst(vindta:426+35:35))
033200111223
033300111223      * Natura merce
033400111223     C                   SELECT
033500111223     c                   WHEN      %subst(vindta:706:1) = 'C'
033600111223     C                   EVAL      VAONAM='COLLI'
033700111223     c                   ENDSL
033800110803     C*
033900111223      * Fermo deposito
034000110803     C                   SELECT
034100110803     C                   WHEN      %subst(vindta:709:1) = '1'
034200110803     C                   EVAL      VAOFFD='S'
034300110803     C                   ENDSL
034400020507     C*
034500020507     C* Reperimento campi NUMERICI
034600020507     C*
034700020527     C* Data e ora apertura chiamata
034800020527     C                   Z-ADD     datcor        VAODAO
034900071016     C                   MOVEL     wn14          VAOOAO
035000020507     C*
035100020507     C* Data ritiro richiesta
035200110803     C                   EVAL      PiStr=%subst(vindta:682:4)+
035300110803     C                                   %subst(vindta:680:2)+
035400110803     C                                   %subst(vindta:678:2)
035500020507     C                   EXSR      CHKNUM
035600020507     C                   IF        PiInt=*on
035700020507     C                   Z-ADD     PiVal         VAODAR
035800020507     C                   ELSE
035900020507     C                   SETON                                        32
036000020507     C                   EVAL      VAODAR = *zeros
036100020507     C                   EVAL      vinmsg = %trimr(vinmsg)
036200020507     C                             + ' ' + 'VAODAR'
036300020507     C                   ENDIF
036400071012     C*
036500071012     C* Colli
036600110803     C                   EVAL      PiStr=%subst(vindta:723:5)
036700071012     C                   EXSR      CHKNUM
036800071012     C                   IF        PiInt=*on
036900071012     C                   Z-ADD     PiVal         VAONCL
037000071012     C                   ELSE
037100071012     C                   SETON                                        32
037200071012     C                   EVAL      VAONCL = *zeros
037300071012     C                   EVAL      vinmsg = %trimr(vinmsg)
037400071012     C                             + ' ' + 'VAONCL'
037500071012     C                   ENDIF
037600071012     C*
037700071012     C* Peso Kg
037800111223     C                   EVAL      PiStr=%subst(vindta:728:4)
037900071012     C                   EXSR      CHKNUM
038000071012     C                   IF        PiNum=*on
038100071012     C                   Z-ADD     PiVal         VAOPKG
038200071012     C                   ELSE
038300071012     C                   SETON                                        32
038400071012     C                   EVAL      VAOPKG = *zeros
038500071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038600071012     C                             + ' ' + 'VAOPKG'
038700071012     C                   ENDIF
038800111223
038900111223      * forzo 1 collo se = 0
039000111223     c                   IF        VAOncl = 0
039100111223     c                   eval      VAOncl = 1
039200111223     c                   ENDIF
039300111223
039400020507     C*
039500020507     C                   ENDSR
039600020507     C*----------------------------------------------------*
039700020507
039800020507
039900020507
040000020507     C*----------------------------------------------------*
040100020507     C*  CONTROLLO NUMERICITA' CAMPI
040200020507     C*----------------------------------------------------*
040300020507     C     CHKNUM        BEGSR
040400020507     C*
040500020507     C                   IF        PiDecChr = *blanks
040600020507     C                   EVAL      PiDecChr = '.'
040700020507     C                   ENDIF
040800020507     C*
040900020507     C                   CALL(e)   'ISNUMERIC'
041000020507     C                   PARM                    PiStr            30
041100020507     C                   PARM                    PiDecChr          1
041200020507     C                   PARM      *ZEROS        PiVal            30 9
041300020507     C                   PARM      '0'           PiInt             1
041400020507     C                   PARM      '0'           PiNum             1
041500020507     C                   IF        %error
041600020507     C                   EVAL      PiNum=*off
041700020507     C                   ENDIF
041800020507     C*
041900020507     C                   ENDSR
042000020507     C***
042100030624
042200030624
042300030624     C*------------------------------------------------------------------------*
042400030624     C* CHKCOR - Considerazioni su VAOCOR
042500030624     C*------------------------------------------------------------------------*
042600030624     C     CHKCOR        BEGSR
042700030624     C*
042800030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
042900030624     C                   if        vaoTCO = 'F'
043000030624     C* Quindi verifico se il codice ordinante non è valorizzato
043100030625     C                   if        vaoCOR = *zeros  AND
043200030625     C                             vaoRSO = *blanks AND
043300030625     C                             vaoINO = *blanks AND
043400030625     C                             vaoCAO = *blanks AND
043500030625     C                             vaoLOO = *blanks AND
043600030625     C                             vaoPRO = *blanks AND
043700030625     C                             vaoNAO = *blanks
043800030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
043900030624     C                   movel     '0'           wFlgAcr           1
044000030624     C                   move      vlrKSC        wCli              7 0
044100030624     C                   move(p)   wCli          acrCRO
044200030624     C     acrCRO        setll     fnacr01l
044300030624     C                   if        %found(fnacr01l)
044400030624     C                   read      fnacr01l
044500030624     C                   dow       not %eof(fnacr01l)
044600030624     C                   movel     acrCRO        wCliAcr           7 0
044700030624     C                   if        wCliAcr = wCli
044800030624     C                   eval      vaoCOR = acrCRO
044900030624     C                   movel     '1'           wFlgAcr
045000030624     C                   leave
045100030624     C                   else
045200030624     C                   leave
045300030624     C                   endif
045400030624     C                   read      fnacr01l
045500030624     C                   enddo
045600030624     C                   endif
045700030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
045800030624     C* dati anagrafici dell'ordinante dal piano dei conti
045900030624     C                   if        wFlgAcr = '0'
046000030624     C                   clear                   BS69DS
046100030624     C                   clear                   ACODS
046200030624     C                   clear                   INDDS
046300030624     C                   clear                   CLPDS
046400030624     C                   clear                   CLSDS
046500030624     C                   move(P)   vlrKSC        I69KAC
046600030624     C                   move(P)   vlrKSC        I69KIN
046700030624     C                   call      'TIBS69R'
046800030624     C                   parm                    BS69DS
046900030624     C                   parm                    ACODS
047000030624     C                   parm                    INDDS
047100030624     C                   parm                    CLPDS
047200030624     C                   parm                    CLSDS
047300030624     C     O69ERR        ifne      '1'
047400030624     C                   eval      vaoRSO = ACORAG
047500030624     C                   eval      vaoINO = INDVIA
047600030624     C                   movel(P)  INDCAP        vaoCAO
047700030624     C                   eval      vaoLOO = INDCIT
047800030624     C                   eval      vaoPRO = INDPRV
047900030624     C                   eval      vaoNAO = INDSTA
048000030624     C                   endif
048100030624     C                   endif
048200030624     C                   endif
048300030624     C                   endif
048400030624     C*
048500030624     C                   ENDSR
048600020507
048700020507
048800010528
048900011105     C*------------------------------------------------------------------------*
049000011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
049100011105     C*------------------------------------------------------------------------*
049200011105     C     WRIORI        BEGSR
049300011105     C*
049400090617     C* Come prima cosa stacco un numeratore da AZNUM
049500090617     C                   clear                   trul33ds
049600090617     C                   eval      I33OPE = *zeros
049700090617     C                   eval      I33CNU = 600
049800090617     C                   eval      I33NUM = 1
049900090617     C                   movel     TRUL33DS      KPJBU
050000090617     C                   call      'TRUL33R'
050100090617     C                   parm                    KPJBA
050200090617     C                   movel     KPJBU         TRUL33DS
050300090617     C                   if        O33ERR = *zeros
050400090617     C                   z-add     O33NRF        §numori
050500090618     c                   else
050600090618     c                   eval      §numori = *all'9'
050700090617     C                   endif
050800011105     C*
050900110502     C* Quindi imposto i campi "extra-VAO"
051000011105     C                   eval      oriPRG = §numori
051100011105     C                   eval      oriDER = datcor
051200110502     C                   eval      oriOER = oracor
051300011105     C                   eval      oriPOE = vaoPOE
051400011105     C                   eval      oriNSR = vaoNSR
051500011105     C                   eval      oriNOR = vaoNOR
051600011105     C                   eval      oriNRV = vaoNRV
051700011105     C                   eval      oriFLO = *blanks
051800011105     C*
051900011122     C* Infine valorizzo la chiave esterna sul file FNVAO
052000011105     C                   eval      dorm01 = vaoFLO
052100011105     C                   movel     §numori       §ormpg
052200110502     C                   movel     'S'           §ormfr
052300111223     C                   movel     'S'           §ormks
052400111223     C                   movel     'S'           §ormfd
052500130306     C***                movel     'N'           §orcomc
052600011105     C                   eval      vaoFLO = dorm01
052700030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
052800030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
052900011105     C*
053000011105     C* ...quindi scrivo il file TIORI00F
053100011105     C                   WRITE     TIORI000
053200011105     C*
053300011105     C                   ENDSR
053400020507     C***
053500050826
053600050826
053700050826
053800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
053900050826     C     wriVGD        BEGSR
054000050826     C*
054100050826     C* Reperisco la descrizione della fase da tabella
054200050826     C                   movel(p)  'FAR'         KteCOD
054300050826     C                   movel(p)  '000'         KteKE1
054400050826     C     KEYtbe        chain     tntbe01l
054500050826     C                   if        %found(tntbe01l)
054600050826     C                   movel(P)  tbeuni        DFAR
054700050826     C                   else
054800050826     C                   clear                   DFAR
054900050826     C                   endif
055000050826     C*
055100050826     C* Valorizzo buffer record
055200050826     C                   clear                   FNVAPDS
055300050826     C                   eval      vapIDC = oriIDC
055400050826     C                   eval      vapPOE = oriPOE
055500050826     C                   eval      vapRFA = vaoRFA
055600050826     C                   eval      vapPOG = oriPOE
055700050826     C                   eval      vapDAE = oriDER
055800050826     C                   eval      vapORE = oriOER
055900050826     C                   if        d§fardva <> *blanks
056000050826     C                   eval      vapDFA = d§fardva
056100050826     C                   else
056200050826     C                   eval      vapDFA = d§fardes
056300050826     C                   endif
056400050826     C*
056500050826     C                   clear                   tivgd000
056600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
056700050826     C                   eval      vgdTIP = 'VP'
056800050826     C                   eval      vgdKSU = vapIDC
056900050826     C                   eval      vgdTSC = 'WW'
057000050826     C                   eval      vgdDAT = datcor
057100151207     C                   eval      vgdPGM = procname
057200050826     C                   write     tivgd000
057300050826     C*
057400050826     C                   ENDSR
057500050826     C*------------------------------------------------------------------------*
057600011119
057700010528
057800010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057900010528     C     repfil        BEGSR
058000010528     C*
058100010528     C                   if        invfil = *zeros and
058200010528     C                             depfil > *zeros and
058300010613     C                             (vinflg = *blanks or
058400010613     C                              vinflg = *zeros)
058500010528     C
058600010528     C                   eval      invfil = depfil
058700010528     C                   endif
058800010528     C*
058900010529     C                   if        depfil <> invfil and
059000010529     C                             invfil > *zeros
059100010528     C                   eval      flgMulti = '1'
059200010528     C                   if        vinflg = *blanks
059300010528     C                   add       1             cntNonEl
059400010528     C                   endif
059500010528     C                   endif
059600010528     C*
059700010528     C                   if        vinflg = '2'
059800010528     C                   eval      flgStato = '2'
059900010528     C                   endif
060000010528     C*
060100010528     C                   ENDSR
060200010528     C***
060300010528
060400010528
060500010528
060600010528
060700990920      /TITLE Invio dei dati al punto operativo.
060800000613     C     invio         BEGSR
060900990920     C*
061000990920     C                   reset                   dscmz
061100010528     C                   move      invfil        cmzdst
061200990920     C                   eval      cmzfld = vlrfou
061300990920     C                   eval      cmzmbd = vlrhdl
061400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061500000710     C                   if        prmfir = *blanks
061600010122     C                   eval      cmzfla = 'FNVAO00F'
061700010122     C                   eval      cmzmba = 'FNVAO00F'
061800000710     C                   else
061900000710     C                   eval      cmzfla = prmfir
062000000710     C                   eval      cmzmba = prmfir
062100000710     C                   endif
062200990920     C                   eval      cmznrr = *zeros
062300990920     C                   move      §ctrok        cmznrr
062400010123     C                   eval      cmzlba = vlrfl1
062500990920     C                   call(e)   'TIS711C'
062600990920     C                   parm                    dscmz
062700990921     C                   parm      *blanks       esito
062800990920     C                   if        %error
062900990920     C                             or cmzerr = '1'
063000990921     C                             or esito  = '1'
063100000710     C                   eval      wrkesito = '3'
063200990920     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600990910
063700010528
063800010528
063900010528
064000010528      /TITLE Invio dei dati al punto operativo.
064100010528     C     opeini        BEGSR
064200010528     C*
064300010528     C* Inizializzo flag e contatori operativi
064400010528     C                   movel     '0'           flgGiro           1
064500010528     C                   movel     '0'           flgMulti          1
064600010528     C                   movel     '1'           flgStato          1
064700010615     C                   movel     '0'           flgOk             1
064800010528     C                   z-add     *zeros        cntNonEl         10 0
064900010528     C                   z-add     *zeros        depfil            3 0
065000010528     C                   z-add     *zeros        invfil            3 0
065100010528     C*
065200010528     C                   ENDSR
065300010528     C***
065400010528
065500010528
065600010528
065700010528
065800000613     C     *inzsr        BEGSR
065900990910     C*
066000990910     C     *entry        plist
066100990920     C                   parm                    tivlrds
066200990921     C                   parm      wrkesito      esito
066300000724     C                   parm                    prmlit
066400000710     C                   parm                    prmfir
066500011105     C*
066600011105     C* Campi riferito al database
066700011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
066800011105     C     *like         define    tbeke1        kteKE1
066900011105     C*
067000011105     C* Definizione chiavi di lettura
067100011105     C* TNTBE01L - parziale
067200011105     C     keytbe        klist
067300011105     C                   kfld                    kteCOD                         *UTENTE
067400011105     C                   kfld                    kteKE1                         *CODICE
067500011105     C*
067600011105     C* Reperimento data e ora corrente
067700110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
067800110502     C                   movel     wn14          oracor            6 0          *ORA (6)
067900110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068000110502     C                   eval      datcor = %dec(%date() : *ISO)
068100010529     C*
068200000613     C                   ENDSR
