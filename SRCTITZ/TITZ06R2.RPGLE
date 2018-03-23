000100120104      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente DUCROS - cod. oriidc2)
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
003700011122     D*-------------------
003800010731     D* COSTANTI
003900011122     D*-------------------
004000010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004100010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004200990908
004300091026
004400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
004500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
004600091026
004700010528
004800010528
004900010528
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000724     C*
005400010528     C                   exsr      opeini
005500010528     C                   exsr      rwvao
005600110701     C*
005700110701     C* Esegue lancio TISI95R solo x chiusura
005800110701     C                   CLEAR                   TISI95DS
005900110701     C                   EVAL      I95TLA = 'C'
006000110701     C                   CALL      'TISI95R'
006100110701     C                   PARM                    TISI95DS
006200010528     C*
006300010528     C                   seton                                        lr
006400010528
006500010528
006600010528
006700010528
006800010528     C*--------------------------------------------------------
006900010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010528     C*--------------------------------------------------------
007100010528     C     PREELA        BEGSR
007200010528     C*
007300000724     C* SE OCCORRE SPEDIRE IN FILIALE
007400010528     C                   if        invfil <> *zeros and
007500010528     C                             flgGiro = '0'
007600010528     C*
007700010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010528     C                   eval      flgGiro = '1'
007900010529     C*
008000010529     C                   endif
008100010528     C*
008200010528     C                   ENDSR
008300010528     C***
008400010528
008500010528
008600010528     C*--------------------------------------------------------
008700010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008800010528     C*--------------------------------------------------------
008900010528     C     ENDELA        BEGSR
009000000613     C*
009100000613     C*
009200010528     C                   ENDSR
009300010528
009400010528
009500910830     C*--------------------------------------------------------
009600020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
009700910830     C*--------------------------------------------------------
009800010122     C     RWVAO         BEGSR
009900010528     C*
010000990914     C                   if        not %open(tivin00r)
010100990908     C                   open      tivin00r
010200990914     C                   endif
010300010122     C                   if        not %open(fnvaowwr)
010400010122     C                   open      fnvaowwr
010500990914     C                   endif
010600990910     C*
010700990910     C                   clear                   §CTROK
010800990910     C                   clear                   §CTRMO
010900990910     C                   clear                   §CTRNO
011000990910     C*
011100921023     C                   DO        *HIVAL
011200990913     C*
011300990915     C                   READ      tivin00r                               70
011400010731     C*
011500010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011600010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
011700010618     C*
011800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011900010618     C                   if        vinflg = '1'
012000010618     C                   eval      flgOk = '1'
012100010618     C                   endif
012200010618     C*
012300000905     C                   if        vindta > *blanks
012400000613     C                   add       1             rrnum
012500990913     C*
012600010711     C                   if        *in70 = *off and
012700010711     C                             (vinflg = *blanks
012800010711     C                              or vinflg = '0'
012900010711     C                              or vinflg = '2')
013000990913     C*
013100010528     C                   clear                   FNVAO000
013200011105     C                   clear                   TIORI000
013300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013400010711     C                   if        vinflg = *blanks or vinflg = '0'
013500010711     C                   clear                   vinmsg
013600010711     C                   endif
013700020507     C*
013800020507     C                   exsr      inzvar
013900020527     C                   exsr      defcam
014000020507     C                   exsr      impvao
014100010122     C*
014200010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014300010528     C                   eval      depfil = VAOPOE
014400010528     C                   exsr      repfil
014500020507     C                   if        depfil = invfil
014600010528     C*
014700010528     C                   exsr      PREELA
014800010710     C*
014900930409     C*
015000010604     C  N31              ADD       1             §CTROK            7 0
015100010604     C   32              ADD       1             §CTRMO            7 0
015200010604     C   31              ADD       1             §CTRNO            7 0
015300020507     C*
015400030624     C  N31              EXSR      CHKCOR
015500011105     C  N31              EXSR      WRIORI
015600110503     C* N31              EXSR      WRIVGD
015700010122     C  N31              WRITE     FNVAO000
015800990910     C*
015900010604     C                   if        *in31 = *off and
016000010604     C                             *in32 = *off
016100990910     C                   eval      vinflg = '1'
016200990910     C                   else
016300990910     C                   eval      vinflg = '2'
016400991022     C                   endif
016500990910     C                   endif
016600010604     C*
016700010604     C                   endif
016800000905     C*
016900000905     C                   else
017000000905     C                   eval      vinflg = '1'
017100000905     C                   endif
017200000905     C*
017300000905     C  N70              update    tivin000
017400991022     C*
017500991022     C  N70              ENDdo
017600010528     C*
017700010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017800010528     C                   if        cntNonEl = *zeros or
017900010528     C                             flgMulti = '0'
018000010528     C* Se non ci sono record con errori ...
018100010528     C                   if        §ctrno = 0 and
018200010604     C                             §ctrmo = 0 and
018300010528     C                             flgStato <> '2'
018400990910     C* ... restituisco esito OK.
018500990921     C                   eval      wrkesito = '0'
018600990910     C                   else
018700000710     C                   if        §ctrok > 0
018800990921     C                   eval      wrkesito = '1'
018900000710     C                   else
019000010615     C                   if        flgOk = '0'
019100010615     C                   eval      wrkesito = '2'
019200010615     C                   else
019300010615     C                   eval      wrkesito = '6'
019400010615     C                   endif
019500990910     C                   endif
019600010528     C                   endif
019700010529     C                   else
019800010529     C                   eval      wrkesito = '9'
019900000710     C                   endif
020000990910     C*
020100990914     C                   if        %open(tivin00r)
020200990908     C                   close     tivin00r
020300990914     C                   endif
020400010122     C                   if        %open(fnvaowwr)
020500010122     C                   close     fnvaowwr
020600990914     C                   endif
020700990910     C*
020800010528     C                   if        vlrpoi <> 999
020900010528     C                   eval      invfil = vlrpoi
021000010528     C                   endif
021100010528     C*
021200990920     C                   if        §ctrok > 0
021300010528     C                             and invfil > *zeros
021400000613     C                   exsr      invio
021500990920     C                   endif
021600010612     C*
021700010612     C                   if        flgGiro = '1'
021800010612     C                   exsr      endela
021900010612     C                   endif
022000010528     C*
022100910830     C                   ENDSR
022200000613     C***
022300990920
022400020507
022500020507     C*----------------------------------------------------*
022600020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022700020507     C*----------------------------------------------------*
022800020507     C     INZVAR        BEGSR
022900020507     C*
023000020507     C                   Z-ADD     *zeros        Num5_0            5 0
023100020507     C                   Z-ADD     *zeros        Num3_0            3 0
023200020507     C*
023300020507     C                   ENDSR
023400020507     C*----------------------------------------------------*
023500020507     C*  IMPOSTAZIONE CAMPI COSTANTI
023600020507     C*----------------------------------------------------*
023700020507     C     DEFCAM        BEGSR
023800020507     C*
023900020507     C* Imposto i valori di default...
024000120329     C                   EVAL      VAOPOE = 330
024100020507     C                   EVAL      VAOTOR = 'S'
024200020507     C                   EVAL      VAOTCO = 'F'
024300120329     C                   EVAL      VAOCOR = 3300025000
024400110502     C                   EVAL      VAOPAG = 'O'
024500120329     C                   EVAL      VAOKSC = 3300025
024600120117     C                   EVAL      VAOCTR = '001'
024700110503     C                   EVAL      VAODDT = 'S'
024800120329     C                   EVAL      VAOPOC = 330
024900110701     C                   EVAL      VAOORR = 1000
025000120329     C                   EVAL      ORIIDC = '03300024'
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
026400020507     C                   Z-ADD     PiVal         VAOPOC
026500020507     C                   ENDIF
026600020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
026700020507     C                   EXSR      CHKNUM
026800020507     C                   IF        PiInt=*on
026900020507     C                   Z-ADD     PiVal         Num3_0
027000020507     C                   MOVEL     NUM3_0        VAOCTR
027100020507     C                   ENDIF
027200020507     C                   ENDIF
027300020507     C*
027400020507     C                   ENDSR
027500020507     C*----------------------------------------------------*
027600020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
027700020507     C*----------------------------------------------------*
027800020507     C     IMPVAO        BEGSR
027900020507     C*
028000020507     C                   SETOFF                                       3132
028100020507     C*
028200020507     C* Reperimento campi ALFA
028300020507     C*
028400110701     C* Dati x il ritiro
028500110701     C                   EVAL      VAORSR=%trim(%subst(vindta:256:35))
028600020507     C* == verifico se esiste il carattere @e, se c'è diventa A
028700020507     C     '@':'A'       XLATE     VAORSR        VAORSR
028800020507     C* ==
028900110701     C                   EVAL      VAOINR=%trim(%subst(vindta:291:35))
029000110701     C                   EVAL      VAOCAR=%trim(%subst(vindta:398:9))
029100110701     C                   EVAL      VAOLOR=%trim(%subst(vindta:407:35))
029200110701     C* Reperisco la provincia dal CAP e dalla località
029300110701     C                   IF        VAOCAR <> *blanks AND
029400110701     C                             VAOPRR  = *blanks AND
029500110701     C                             VAONAR  = *blanks
029600110701     C                   CLEAR                   TISI95DS
029700110701     C                   EVAL      I95TCN = '3'
029800110701     C                   Z-ADD     datcor        I95DAT
029900110701     C                   EVAL      I95CAP = VAOCAR
030000110701     C                   EVAL      I95LOC = VAOLOR
030100110701     C                   CALL      'TISI95R'
030200110701     C                   PARM                    TISI95DS
030300110701     C                   EVAL      VAOPRR = O95PRV
030400110701     C                   ENDIF
030500110502     C*
030600110701     C* Dati x la consegna
030700111228     C                   EVAL      VAORSC=%trim(%subst(vindta:468:35))
030800110502     C* == verifico se esiste il carattere @e, se c'è diventa A
030900110502     C     '@':'A'       XLATE     VAORSC        VAORSC
031000110502     C* ==
031100111228     C                   EVAL      VAOINC=%trim(%subst(vindta:503:35))+
031200111228     C                                    %trim(%subst(vindta:538:35))+
031300111228     C                                    %trim(%subst(vindta:573:35))
031400111228     C                   EVAL      VAOCAC=%trim(%subst(vindta:610:9))
031500111228     C                   EVAL      VAOLOC=%trim(%subst(vindta:619:35))
031600111228     C                   EVAL      VAONAC = 'F'
031700110502     C*
031800110701     C* Dati genericio ORM
031900110701     C                   EVAL      VAORER=%trim(%subst(vindta:657:17))
032000110701     C                   EVAL      VAOTER=%trim(%subst(vindta:674:15))
032100110701     C                   EVAL      VAORFA=%trim(%subst(vindta:1028:10))
032200110711     C                   EVAL      VAONAM=%trim(%subst(vindta:753:20))
032300110701     C                   MOVEL     *blanks       wNOTE            70
032400110706     C                   EVAL      wNOTE ='Ritiro per conto: ' +
032500110706     C                                 %trim(%subst(vindta:468:35))            +
032600110706     C                               ' ('+%trim(%subst(vindta:442:3)) +') Rif:'+
032700110706     C                                 %trim(%subst(vindta:1074:15))
032800110706     C                   EVAL      VAONO1=%trim(%subst(wNOTE:1:35))
032900110706     C                   EVAL      VAONO2=%trim(%subst(wNOTE:36:35))
033000020507     C*
033100020507     C* Reperimento campi NUMERICI
033200020507     C*
033300020527     C* Data e ora apertura chiamata
033400020527     C                   Z-ADD     datcor        VAODAO
033500071016     C                   MOVEL     wn14          VAOOAO
033600020507     C*
033700020507     C* Data ritiro richiesta
033800110701     C                   EVAL      PiStr=%subst(vindta:697:8)
033900020507     C                   EXSR      CHKNUM
034000020507     C                   IF        PiInt=*on
034100020507     C                   Z-ADD     PiVal         VAODAR
034200020507     C                   ELSE
034300020507     C                   SETON                                        32
034400020507     C                   EVAL      VAODAR = *zeros
034500020507     C                   EVAL      vinmsg = %trimr(vinmsg)
034600020507     C                             + ' ' + 'VAODAR'
034700020507     C                   ENDIF
034800110701     C*
034900110701     C* Ora ritiro richiesta
035000110701     C                   EVAL      PiStr=%subst(vindta:705:4)
035100110701     C                   EXSR      CHKNUM
035200110701     C                   IF        PiInt=*on
035300110701     C                   IF        PiVal > *zeros
035400110701     C                   Z-ADD     PiVal         VAOORR
035500110701     C                   ENDIF
035600110701     C                   ELSE
035700110701     C                   SETON                                        32
035800110701     C                   EVAL      VAOORR = *zeros
035900110701     C                   EVAL      vinmsg = %trimr(vinmsg)
036000110701     C                             + ' ' + 'VAOORR'
036100110701     C                   ENDIF
036200071012     C*
036300071012     C* Colli
036400110701     C                   EVAL      PiStr=%subst(vindta:729:5)
036500071012     C                   EXSR      CHKNUM
036600071012     C                   IF        PiInt=*on
036700071012     C                   Z-ADD     PiVal         VAONCL
036800071012     C                   ELSE
036900071012     C                   SETON                                        32
037000071012     C                   EVAL      VAONCL = *zeros
037100071012     C                   EVAL      vinmsg = %trimr(vinmsg)
037200071012     C                             + ' ' + 'VAONCL'
037300071012     C                   ENDIF
037400071012     C*
037500071012     C* Peso Kg
037600110701     C                   EVAL      PiStr=%subst(vindta:734:9)
037700071012     C                   EXSR      CHKNUM
037800071012     C                   IF        PiNum=*on
037900071012     C                   Z-ADD     PiVal         VAOPKG
038000110701     C                   IF        VAOPKG = *zeros
038100110701     C                   Z-ADD     0,1           VAOPKG
038200110701     C                   ENDIF
038300071012     C                   ELSE
038400071012     C                   SETON                                        32
038500071012     C                   EVAL      VAOPKG = *zeros
038600071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038700071012     C                             + ' ' + 'VAOPKG'
038800071012     C                   ENDIF
038900110502     C*
039000110502     C* Volume M3
039100110701     C                   EVAL      PiStr=%subst(vindta:743:4)
039200110502     C                   EXSR      CHKNUM
039300110502     C                   IF        PiNum=*on
039400110502     C                   Z-ADD     PiVal         VAOVLM
039500110502     C                   ELSE
039600110502     C                   SETON                                        32
039700110502     C                   EVAL      VAOVLM = *zeros
039800110502     C                   EVAL      vinmsg = %trimr(vinmsg)
039900110502     C                             + ' ' + 'VAOVLM'
040000110502     C                   ENDIF
040100020507     C*
040200020507     C                   ENDSR
040300020507     C*----------------------------------------------------*
040400020507
040500020507
040600020507
040700020507     C*----------------------------------------------------*
040800020507     C*  CONTROLLO NUMERICITA' CAMPI
040900020507     C*----------------------------------------------------*
041000020507     C     CHKNUM        BEGSR
041100020507     C*
041200020507     C                   IF        PiDecChr = *blanks
041300020507     C                   EVAL      PiDecChr = '.'
041400020507     C                   ENDIF
041500020507     C*
041600020507     C                   CALL(e)   'ISNUMERIC'
041700020507     C                   PARM                    PiStr            30
041800020507     C                   PARM                    PiDecChr          1
041900020507     C                   PARM      *ZEROS        PiVal            30 9
042000020507     C                   PARM      '0'           PiInt             1
042100020507     C                   PARM      '0'           PiNum             1
042200020507     C                   IF        %error
042300020507     C                   EVAL      PiNum=*off
042400020507     C                   ENDIF
042500020507     C*
042600020507     C                   ENDSR
042700020507     C***
042800030624
042900030624
043000030624     C*------------------------------------------------------------------------*
043100030624     C* CHKCOR - Considerazioni su VAOCOR
043200030624     C*------------------------------------------------------------------------*
043300030624     C     CHKCOR        BEGSR
043400030624     C*
043500030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
043600030624     C                   if        vaoTCO = 'F'
043700030624     C* Quindi verifico se il codice ordinante non è valorizzato
043800030625     C                   if        vaoCOR = *zeros  AND
043900030625     C                             vaoRSO = *blanks AND
044000030625     C                             vaoINO = *blanks AND
044100030625     C                             vaoCAO = *blanks AND
044200030625     C                             vaoLOO = *blanks AND
044300030625     C                             vaoPRO = *blanks AND
044400030625     C                             vaoNAO = *blanks
044500030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
044600030624     C                   movel     '0'           wFlgAcr           1
044700030624     C                   move      vlrKSC        wCli              7 0
044800030624     C                   move(p)   wCli          acrCRO
044900030624     C     acrCRO        setll     fnacr01l
045000030624     C                   if        %found(fnacr01l)
045100030624     C                   read      fnacr01l
045200030624     C                   dow       not %eof(fnacr01l)
045300030624     C                   movel     acrCRO        wCliAcr           7 0
045400030624     C                   if        wCliAcr = wCli
045500030624     C                   eval      vaoCOR = acrCRO
045600030624     C                   movel     '1'           wFlgAcr
045700030624     C                   leave
045800030624     C                   else
045900030624     C                   leave
046000030624     C                   endif
046100030624     C                   read      fnacr01l
046200030624     C                   enddo
046300030624     C                   endif
046400030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
046500030624     C* dati anagrafici dell'ordinante dal piano dei conti
046600030624     C                   if        wFlgAcr = '0'
046700030624     C                   clear                   BS69DS
046800030624     C                   clear                   ACODS
046900030624     C                   clear                   INDDS
047000030624     C                   clear                   CLPDS
047100030624     C                   clear                   CLSDS
047200030624     C                   move(P)   vlrKSC        I69KAC
047300030624     C                   move(P)   vlrKSC        I69KIN
047400030624     C                   call      'TIBS69R'
047500030624     C                   parm                    BS69DS
047600030624     C                   parm                    ACODS
047700030624     C                   parm                    INDDS
047800030624     C                   parm                    CLPDS
047900030624     C                   parm                    CLSDS
048000030624     C     O69ERR        ifne      '1'
048100030624     C                   eval      vaoRSO = ACORAG
048200030624     C                   eval      vaoINO = INDVIA
048300030624     C                   movel(P)  INDCAP        vaoCAO
048400030624     C                   eval      vaoLOO = INDCIT
048500030624     C                   eval      vaoPRO = INDPRV
048600030624     C                   eval      vaoNAO = INDSTA
048700030624     C                   endif
048800030624     C                   endif
048900030624     C                   endif
049000030624     C                   endif
049100030624     C*
049200030624     C                   ENDSR
049300020507
049400020507
049500010528
049600011105     C*------------------------------------------------------------------------*
049700011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
049800011105     C*------------------------------------------------------------------------*
049900011105     C     WRIORI        BEGSR
050000011105     C*
050100090617     C* Come prima cosa stacco un numeratore da AZNUM
050200090617     C                   clear                   trul33ds
050300090617     C                   eval      I33OPE = *zeros
050400090617     C                   eval      I33CNU = 600
050500090617     C                   eval      I33NUM = 1
050600090617     C                   movel     TRUL33DS      KPJBU
050700090617     C                   call      'TRUL33R'
050800090617     C                   parm                    KPJBA
050900090617     C                   movel     KPJBU         TRUL33DS
051000090617     C                   if        O33ERR = *zeros
051100090617     C                   z-add     O33NRF        §numori
051200090618     c                   else
051300090618     c                   eval      §numori = *all'9'
051400090617     C                   endif
051500011105     C*
051600110502     C* Quindi imposto i campi "extra-VAO"
051700011105     C                   eval      oriPRG = §numori
051800011105     C                   eval      oriDER = datcor
051900110502     C                   eval      oriOER = oracor
052000011105     C                   eval      oriPOE = vaoPOE
052100011105     C                   eval      oriNSR = vaoNSR
052200011105     C                   eval      oriNOR = vaoNOR
052300011105     C                   eval      oriNRV = vaoNRV
052400011105     C                   eval      oriFLO = *blanks
052500011105     C*
052600011122     C* Infine valorizzo la chiave esterna sul file FNVAO
052700011105     C                   eval      dorm01 = vaoFLO
052800011105     C                   movel     §numori       §ormpg
052900110502     C                   movel     'S'           §ormfr
053000110502     C                   movel     'S'           §ormks
053100110502     C                   movel     'S'           §ormfd
053200011105     C                   eval      vaoFLO = dorm01
053300030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
053400030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
053500011105     C*
053600011105     C* ...quindi scrivo il file TIORI00F
053700011105     C                   WRITE     TIORI000
053800011105     C*
053900011105     C                   ENDSR
054000020507     C***
054100050826
054200050826
054300050826
054400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054500050826     C     wriVGD        BEGSR
054600050826     C*
054700050826     C* Reperisco la descrizione della fase da tabella
054800050826     C                   movel(p)  'FAR'         KteCOD
054900050826     C                   movel(p)  '000'         KteKE1
055000050826     C     KEYtbe        chain     tntbe01l
055100050826     C                   if        %found(tntbe01l)
055200050826     C                   movel(P)  tbeuni        DFAR
055300050826     C                   else
055400050826     C                   clear                   DFAR
055500050826     C                   endif
055600050826     C*
055700050826     C* Valorizzo buffer record
055800050826     C                   clear                   FNVAPDS
055900050826     C                   eval      vapIDC = oriIDC
056000050826     C                   eval      vapPOE = oriPOE
056100050826     C                   eval      vapRFA = vaoRFA
056200050826     C                   eval      vapPOG = oriPOE
056300050826     C                   eval      vapDAE = oriDER
056400050826     C                   eval      vapORE = oriOER
056500050826     C                   if        d§fardva <> *blanks
056600050826     C                   eval      vapDFA = d§fardva
056700050826     C                   else
056800050826     C                   eval      vapDFA = d§fardes
056900050826     C                   endif
057000050826     C*
057100050826     C                   clear                   tivgd000
057200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057300050826     C                   eval      vgdTIP = 'VP'
057400050826     C                   eval      vgdKSU = vapIDC
057500050826     C                   eval      vgdTSC = 'WW'
057600050826     C                   eval      vgdDAT = datcor
057700151207     C                   eval      vgdPGM = procname
057800050826     C                   write     tivgd000
057900050826     C*
058000050826     C                   ENDSR
058100050826     C*------------------------------------------------------------------------*
058200011119
058300010528
058400010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058500010528     C     repfil        BEGSR
058600010528     C*
058700010528     C                   if        invfil = *zeros and
058800010528     C                             depfil > *zeros and
058900010613     C                             (vinflg = *blanks or
059000010613     C                              vinflg = *zeros)
059100010528     C
059200010528     C                   eval      invfil = depfil
059300010528     C                   endif
059400010528     C*
059500010529     C                   if        depfil <> invfil and
059600010529     C                             invfil > *zeros
059700010528     C                   eval      flgMulti = '1'
059800010528     C                   if        vinflg = *blanks
059900010528     C                   add       1             cntNonEl
060000010528     C                   endif
060100010528     C                   endif
060200010528     C*
060300010528     C                   if        vinflg = '2'
060400010528     C                   eval      flgStato = '2'
060500010528     C                   endif
060600010528     C*
060700010528     C                   ENDSR
060800010528     C***
060900010528
061000010528
061100010528
061200010528
061300990920      /TITLE Invio dei dati al punto operativo.
061400000613     C     invio         BEGSR
061500990920     C*
061600990920     C                   reset                   dscmz
061700010528     C                   move      invfil        cmzdst
061800990920     C                   eval      cmzfld = vlrfou
061900990920     C                   eval      cmzmbd = vlrhdl
062000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
062100000710     C                   if        prmfir = *blanks
062200010122     C                   eval      cmzfla = 'FNVAO00F'
062300010122     C                   eval      cmzmba = 'FNVAO00F'
062400000710     C                   else
062500000710     C                   eval      cmzfla = prmfir
062600000710     C                   eval      cmzmba = prmfir
062700000710     C                   endif
062800990920     C                   eval      cmznrr = *zeros
062900990920     C                   move      §ctrok        cmznrr
063000010123     C                   eval      cmzlba = vlrfl1
063100990920     C                   call(e)   'TIS711C'
063200990920     C                   parm                    dscmz
063300990921     C                   parm      *blanks       esito
063400990920     C                   if        %error
063500990920     C                             or cmzerr = '1'
063600990921     C                             or esito  = '1'
063700000710     C                   eval      wrkesito = '3'
063800990920     C                   endif
063900990920     C*
064000000613     C                   ENDSR
064100000613     C***
064200990910
064300010528
064400010528
064500010528
064600010528      /TITLE Invio dei dati al punto operativo.
064700010528     C     opeini        BEGSR
064800010528     C*
064900010528     C* Inizializzo flag e contatori operativi
065000010528     C                   movel     '0'           flgGiro           1
065100010528     C                   movel     '0'           flgMulti          1
065200010528     C                   movel     '1'           flgStato          1
065300010615     C                   movel     '0'           flgOk             1
065400010528     C                   z-add     *zeros        cntNonEl         10 0
065500010528     C                   z-add     *zeros        depfil            3 0
065600010528     C                   z-add     *zeros        invfil            3 0
065700010528     C*
065800010528     C                   ENDSR
065900010528     C***
066000010528
066100010528
066200010528
066300010528
066400000613     C     *inzsr        BEGSR
066500990910     C*
066600990910     C     *entry        plist
066700990920     C                   parm                    tivlrds
066800990921     C                   parm      wrkesito      esito
066900000724     C                   parm                    prmlit
067000000710     C                   parm                    prmfir
067100011105     C*
067200011105     C* Campi riferito al database
067300011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
067400011105     C     *like         define    tbeke1        kteKE1
067500011105     C*
067600011105     C* Definizione chiavi di lettura
067700011105     C* TNTBE01L - parziale
067800011105     C     keytbe        klist
067900011105     C                   kfld                    kteCOD                         *UTENTE
068000011105     C                   kfld                    kteKE1                         *CODICE
068100011105     C*
068200011105     C* Reperimento data e ora corrente
068300110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
068400110502     C                   movel     wn14          oracor            6 0          *ORA (6)
068500110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068600110502     C                   eval      datcor = %dec(%date() : *ISO)
068700010529     C*
068800000613     C                   ENDSR
