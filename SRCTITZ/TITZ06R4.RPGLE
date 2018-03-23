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
024300140402     C                   EVAL      VAOCOR = 3300036000
024400110502     C                   EVAL      VAOPAG = 'O'
024500140402     C                   EVAL      VAOKSC = 3300036
024600120117     C                   EVAL      VAOCTR = '001'
024700110503     C                   EVAL      VAODDT = 'S'
024800120329     C                   EVAL      VAOPOC = 330
024900110701     C                   EVAL      VAOORR = 1000
025000140402     C                   EVAL      ORIIDC = '03300036'
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
028900140910     C                   EVAL      VAOINR=%trim(%subst(vindta:291:35))+' '+
029000140910     C                                    %trim(%subst(vindta:326:35))+' '+
029100140910     C                                    %trim(%subst(vindta:361:35))
029200110701     C                   EVAL      VAOCAR=%trim(%subst(vindta:398:9))
029300110701     C                   EVAL      VAOLOR=%trim(%subst(vindta:407:35))
029400110701     C* Reperisco la provincia dal CAP e dalla località
029500110701     C                   IF        VAOCAR <> *blanks AND
029600110701     C                             VAOPRR  = *blanks AND
029700110701     C                             VAONAR  = *blanks
029800110701     C                   CLEAR                   TISI95DS
029900110701     C                   EVAL      I95TCN = '3'
030000110701     C                   Z-ADD     datcor        I95DAT
030100110701     C                   EVAL      I95CAP = VAOCAR
030200110701     C                   EVAL      I95LOC = VAOLOR
030300110701     C                   CALL      'TISI95R'
030400110701     C                   PARM                    TISI95DS
030500110701     C                   EVAL      VAOPRR = O95PRV
030600110701     C                   ENDIF
030700110502     C*
030800110701     C* Dati x la consegna
030900111228     C                   EVAL      VAORSC=%trim(%subst(vindta:468:35))
031000110502     C* == verifico se esiste il carattere @e, se c'è diventa A
031100110502     C     '@':'A'       XLATE     VAORSC        VAORSC
031200110502     C* ==
031300111228     C                   EVAL      VAOINC=%trim(%subst(vindta:503:35))+
031400111228     C                                    %trim(%subst(vindta:538:35))+
031500111228     C                                    %trim(%subst(vindta:573:35))
031600111228     C                   EVAL      VAOCAC=%trim(%subst(vindta:610:9))
031700111228     C                   EVAL      VAOLOC=%trim(%subst(vindta:619:35))
031800111228     C                   EVAL      VAONAC = 'F'
031900110502     C*
032000110701     C* Dati genericio ORM
032100110701     C                   EVAL      VAORER=%trim(%subst(vindta:657:17))
032200110701     C                   EVAL      VAOTER=%trim(%subst(vindta:674:15))
032300131211     C                   EVAL      VAORFA=%trim(%subst(vindta:1074:10))
032400110711     C                   EVAL      VAONAM=%trim(%subst(vindta:753:20))
032500110701     C                   MOVEL     *blanks       wNOTE            70
032600110706     C                   EVAL      wNOTE ='Ritiro per conto: ' +
032700110706     C                                 %trim(%subst(vindta:468:35))            +
032800110706     C                               ' ('+%trim(%subst(vindta:442:3)) +') Rif:'+
032900110706     C                                 %trim(%subst(vindta:1074:15))
033000110706     C                   EVAL      VAONO1=%trim(%subst(wNOTE:1:35))
033100110706     C                   EVAL      VAONO2=%trim(%subst(wNOTE:36:35))
033200020507     C*
033300020507     C* Reperimento campi NUMERICI
033400020507     C*
033500020527     C* Data e ora apertura chiamata
033600020527     C                   Z-ADD     datcor        VAODAO
033700071016     C                   MOVEL     wn14          VAOOAO
033800020507     C*
033900020507     C* Data ritiro richiesta
034000110701     C                   EVAL      PiStr=%subst(vindta:697:8)
034100020507     C                   EXSR      CHKNUM
034200020507     C                   IF        PiInt=*on
034300020507     C                   Z-ADD     PiVal         VAODAR
034400020507     C                   ELSE
034500020507     C                   SETON                                        32
034600020507     C                   EVAL      VAODAR = *zeros
034700020507     C                   EVAL      vinmsg = %trimr(vinmsg)
034800020507     C                             + ' ' + 'VAODAR'
034900020507     C                   ENDIF
035000110701     C*
035100110701     C* Ora ritiro richiesta
035200110701     C                   EVAL      PiStr=%subst(vindta:705:4)
035300110701     C                   EXSR      CHKNUM
035400110701     C                   IF        PiInt=*on
035500110701     C                   IF        PiVal > *zeros
035600110701     C                   Z-ADD     PiVal         VAOORR
035700110701     C                   ENDIF
035800110701     C                   ELSE
035900110701     C                   SETON                                        32
036000110701     C                   EVAL      VAOORR = *zeros
036100110701     C                   EVAL      vinmsg = %trimr(vinmsg)
036200110701     C                             + ' ' + 'VAOORR'
036300110701     C                   ENDIF
036400071012     C*
036500071012     C* Colli
036600110701     C                   EVAL      PiStr=%subst(vindta:729:5)
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
037800110701     C                   EVAL      PiStr=%subst(vindta:734:9)
037900071012     C                   EXSR      CHKNUM
038000071012     C                   IF        PiNum=*on
038100071012     C                   Z-ADD     PiVal         VAOPKG
038200110701     C                   IF        VAOPKG = *zeros
038300110701     C                   Z-ADD     0,1           VAOPKG
038400110701     C                   ENDIF
038500071012     C                   ELSE
038600071012     C                   SETON                                        32
038700071012     C                   EVAL      VAOPKG = *zeros
038800071012     C                   EVAL      vinmsg = %trimr(vinmsg)
038900071012     C                             + ' ' + 'VAOPKG'
039000071012     C                   ENDIF
039100110502     C*
039200110502     C* Volume M3
039300110701     C                   EVAL      PiStr=%subst(vindta:743:4)
039400110502     C                   EXSR      CHKNUM
039500110502     C                   IF        PiNum=*on
039600110502     C                   Z-ADD     PiVal         VAOVLM
039700110502     C                   ELSE
039800110502     C                   SETON                                        32
039900110502     C                   EVAL      VAOVLM = *zeros
040000110502     C                   EVAL      vinmsg = %trimr(vinmsg)
040100110502     C                             + ' ' + 'VAOVLM'
040200110502     C                   ENDIF
040300020507     C*
040400020507     C                   ENDSR
040500020507     C*----------------------------------------------------*
040600020507
040700020507
040800020507
040900020507     C*----------------------------------------------------*
041000020507     C*  CONTROLLO NUMERICITA' CAMPI
041100020507     C*----------------------------------------------------*
041200020507     C     CHKNUM        BEGSR
041300020507     C*
041400020507     C                   IF        PiDecChr = *blanks
041500020507     C                   EVAL      PiDecChr = '.'
041600020507     C                   ENDIF
041700020507     C*
041800020507     C                   CALL(e)   'ISNUMERIC'
041900020507     C                   PARM                    PiStr            30
042000020507     C                   PARM                    PiDecChr          1
042100020507     C                   PARM      *ZEROS        PiVal            30 9
042200020507     C                   PARM      '0'           PiInt             1
042300020507     C                   PARM      '0'           PiNum             1
042400020507     C                   IF        %error
042500020507     C                   EVAL      PiNum=*off
042600020507     C                   ENDIF
042700020507     C*
042800020507     C                   ENDSR
042900020507     C***
043000030624
043100030624
043200030624     C*------------------------------------------------------------------------*
043300030624     C* CHKCOR - Considerazioni su VAOCOR
043400030624     C*------------------------------------------------------------------------*
043500030624     C     CHKCOR        BEGSR
043600030624     C*
043700030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
043800030624     C                   if        vaoTCO = 'F'
043900030624     C* Quindi verifico se il codice ordinante non è valorizzato
044000030625     C                   if        vaoCOR = *zeros  AND
044100030625     C                             vaoRSO = *blanks AND
044200030625     C                             vaoINO = *blanks AND
044300030625     C                             vaoCAO = *blanks AND
044400030625     C                             vaoLOO = *blanks AND
044500030625     C                             vaoPRO = *blanks AND
044600030625     C                             vaoNAO = *blanks
044700030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
044800030624     C                   movel     '0'           wFlgAcr           1
044900030624     C                   move      vlrKSC        wCli              7 0
045000030624     C                   move(p)   wCli          acrCRO
045100030624     C     acrCRO        setll     fnacr01l
045200030624     C                   if        %found(fnacr01l)
045300030624     C                   read      fnacr01l
045400030624     C                   dow       not %eof(fnacr01l)
045500030624     C                   movel     acrCRO        wCliAcr           7 0
045600030624     C                   if        wCliAcr = wCli
045700030624     C                   eval      vaoCOR = acrCRO
045800030624     C                   movel     '1'           wFlgAcr
045900030624     C                   leave
046000030624     C                   else
046100030624     C                   leave
046200030624     C                   endif
046300030624     C                   read      fnacr01l
046400030624     C                   enddo
046500030624     C                   endif
046600030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
046700030624     C* dati anagrafici dell'ordinante dal piano dei conti
046800030624     C                   if        wFlgAcr = '0'
046900030624     C                   clear                   BS69DS
047000030624     C                   clear                   ACODS
047100030624     C                   clear                   INDDS
047200030624     C                   clear                   CLPDS
047300030624     C                   clear                   CLSDS
047400030624     C                   move(P)   vlrKSC        I69KAC
047500030624     C                   move(P)   vlrKSC        I69KIN
047600030624     C                   call      'TIBS69R'
047700030624     C                   parm                    BS69DS
047800030624     C                   parm                    ACODS
047900030624     C                   parm                    INDDS
048000030624     C                   parm                    CLPDS
048100030624     C                   parm                    CLSDS
048200030624     C     O69ERR        ifne      '1'
048300030624     C                   eval      vaoRSO = ACORAG
048400030624     C                   eval      vaoINO = INDVIA
048500030624     C                   movel(P)  INDCAP        vaoCAO
048600030624     C                   eval      vaoLOO = INDCIT
048700030624     C                   eval      vaoPRO = INDPRV
048800030624     C                   eval      vaoNAO = INDSTA
048900030624     C                   endif
049000030624     C                   endif
049100030624     C                   endif
049200030624     C                   endif
049300030624     C*
049400030624     C                   ENDSR
049500020507
049600020507
049700010528
049800011105     C*------------------------------------------------------------------------*
049900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
050000011105     C*------------------------------------------------------------------------*
050100011105     C     WRIORI        BEGSR
050200011105     C*
050300090617     C* Come prima cosa stacco un numeratore da AZNUM
050400090617     C                   clear                   trul33ds
050500090617     C                   eval      I33OPE = *zeros
050600090617     C                   eval      I33CNU = 600
050700090617     C                   eval      I33NUM = 1
050800090617     C                   movel     TRUL33DS      KPJBU
050900090617     C                   call      'TRUL33R'
051000090617     C                   parm                    KPJBA
051100090617     C                   movel     KPJBU         TRUL33DS
051200090617     C                   if        O33ERR = *zeros
051300090617     C                   z-add     O33NRF        §numori
051400090618     c                   else
051500090618     c                   eval      §numori = *all'9'
051600090617     C                   endif
051700011105     C*
051800110502     C* Quindi imposto i campi "extra-VAO"
051900011105     C                   eval      oriPRG = §numori
052000011105     C                   eval      oriDER = datcor
052100110502     C                   eval      oriOER = oracor
052200011105     C                   eval      oriPOE = vaoPOE
052300011105     C                   eval      oriNSR = vaoNSR
052400011105     C                   eval      oriNOR = vaoNOR
052500011105     C                   eval      oriNRV = vaoNRV
052600011105     C                   eval      oriFLO = *blanks
052700011105     C*
052800011122     C* Infine valorizzo la chiave esterna sul file FNVAO
052900011105     C                   eval      dorm01 = vaoFLO
053000011105     C                   movel     §numori       §ormpg
053100110502     C                   movel     'S'           §ormfr
053200110502     C                   movel     'S'           §ormks
053300110502     C                   movel     'S'           §ormfd
053400011105     C                   eval      vaoFLO = dorm01
053500030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
053600030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
053700011105     C*
053800011105     C* ...quindi scrivo il file TIORI00F
053900011105     C                   WRITE     TIORI000
054000011105     C*
054100011105     C                   ENDSR
054200020507     C***
054300050826
054400050826
054500050826
054600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054700050826     C     wriVGD        BEGSR
054800050826     C*
054900050826     C* Reperisco la descrizione della fase da tabella
055000050826     C                   movel(p)  'FAR'         KteCOD
055100050826     C                   movel(p)  '000'         KteKE1
055200050826     C     KEYtbe        chain     tntbe01l
055300050826     C                   if        %found(tntbe01l)
055400050826     C                   movel(P)  tbeuni        DFAR
055500050826     C                   else
055600050826     C                   clear                   DFAR
055700050826     C                   endif
055800050826     C*
055900050826     C* Valorizzo buffer record
056000050826     C                   clear                   FNVAPDS
056100050826     C                   eval      vapIDC = oriIDC
056200050826     C                   eval      vapPOE = oriPOE
056300050826     C                   eval      vapRFA = vaoRFA
056400050826     C                   eval      vapPOG = oriPOE
056500050826     C                   eval      vapDAE = oriDER
056600050826     C                   eval      vapORE = oriOER
056700050826     C                   if        d§fardva <> *blanks
056800050826     C                   eval      vapDFA = d§fardva
056900050826     C                   else
057000050826     C                   eval      vapDFA = d§fardes
057100050826     C                   endif
057200050826     C*
057300050826     C                   clear                   tivgd000
057400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057500050826     C                   eval      vgdTIP = 'VP'
057600050826     C                   eval      vgdKSU = vapIDC
057700050826     C                   eval      vgdTSC = 'WW'
057800050826     C                   eval      vgdDAT = datcor
057900151207     C                   eval      vgdPGM = procname
058000050826     C                   write     tivgd000
058100050826     C*
058200050826     C                   ENDSR
058300050826     C*------------------------------------------------------------------------*
058400011119
058500010528
058600010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058700010528     C     repfil        BEGSR
058800010528     C*
058900010528     C                   if        invfil = *zeros and
059000010528     C                             depfil > *zeros and
059100010613     C                             (vinflg = *blanks or
059200010613     C                              vinflg = *zeros)
059300010528     C
059400010528     C                   eval      invfil = depfil
059500010528     C                   endif
059600010528     C*
059700010529     C                   if        depfil <> invfil and
059800010529     C                             invfil > *zeros
059900010528     C                   eval      flgMulti = '1'
060000010528     C                   if        vinflg = *blanks
060100010528     C                   add       1             cntNonEl
060200010528     C                   endif
060300010528     C                   endif
060400010528     C*
060500010528     C                   if        vinflg = '2'
060600010528     C                   eval      flgStato = '2'
060700010528     C                   endif
060800010528     C*
060900010528     C                   ENDSR
061000010528     C***
061100010528
061200010528
061300010528
061400010528
061500990920      /TITLE Invio dei dati al punto operativo.
061600000613     C     invio         BEGSR
061700990920     C*
061800990920     C                   reset                   dscmz
061900010528     C                   move      invfil        cmzdst
062000990920     C                   eval      cmzfld = vlrfou
062100990920     C                   eval      cmzmbd = vlrhdl
062200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300000710     C                   if        prmfir = *blanks
062400010122     C                   eval      cmzfla = 'FNVAO00F'
062500010122     C                   eval      cmzmba = 'FNVAO00F'
062600000710     C                   else
062700000710     C                   eval      cmzfla = prmfir
062800000710     C                   eval      cmzmba = prmfir
062900000710     C                   endif
063000990920     C                   eval      cmznrr = *zeros
063100990920     C                   move      §ctrok        cmznrr
063200010123     C                   eval      cmzlba = vlrfl1
063300990920     C                   call(e)   'TIS711C'
063400990920     C                   parm                    dscmz
063500990921     C                   parm      *blanks       esito
063600990920     C                   if        %error
063700990920     C                             or cmzerr = '1'
063800990921     C                             or esito  = '1'
063900000710     C                   eval      wrkesito = '3'
064000990920     C                   endif
064100990920     C*
064200000613     C                   ENDSR
064300000613     C***
064400990910
064500010528
064600010528
064700010528
064800010528      /TITLE Invio dei dati al punto operativo.
064900010528     C     opeini        BEGSR
065000010528     C*
065100010528     C* Inizializzo flag e contatori operativi
065200010528     C                   movel     '0'           flgGiro           1
065300010528     C                   movel     '0'           flgMulti          1
065400010528     C                   movel     '1'           flgStato          1
065500010615     C                   movel     '0'           flgOk             1
065600010528     C                   z-add     *zeros        cntNonEl         10 0
065700010528     C                   z-add     *zeros        depfil            3 0
065800010528     C                   z-add     *zeros        invfil            3 0
065900010528     C*
066000010528     C                   ENDSR
066100010528     C***
066200010528
066300010528
066400010528
066500010528
066600000613     C     *inzsr        BEGSR
066700990910     C*
066800990910     C     *entry        plist
066900990920     C                   parm                    tivlrds
067000990921     C                   parm      wrkesito      esito
067100000724     C                   parm                    prmlit
067200000710     C                   parm                    prmfir
067300011105     C*
067400011105     C* Campi riferito al database
067500011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
067600011105     C     *like         define    tbeke1        kteKE1
067700011105     C*
067800011105     C* Definizione chiavi di lettura
067900011105     C* TNTBE01L - parziale
068000011105     C     keytbe        klist
068100011105     C                   kfld                    kteCOD                         *UTENTE
068200011105     C                   kfld                    kteKE1                         *CODICE
068300011105     C*
068400011105     C* Reperimento data e ora corrente
068500110502     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
068600110502     C                   movel     wn14          oracor            6 0          *ORA (6)
068700110502     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068800110502     C                   eval      datcor = %dec(%date() : *ISO)
068900010529     C*
069000000613     C                   ENDSR
