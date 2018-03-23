000100020527      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente S.G. Cod. 0632142)
000200990908     H dftactgrp(*yes)
000300990908
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
002900090617     d trul33ds      e ds
003000090617     d kpjba         e ds
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600090617     d §numori         s              7  0 inz
003700011105     D*-------------------
003800011105     D* DS "XSRDA8" - controllo data (8)
003900011105     D*-------------------
004000011105     D wlbda8          DS                  INZ
004100011105     D  g08dat                 1      8  0
004200011105     D  g08inv                 9     16  0
004300011105     D  g08err                17     17
004400011105     D  g08tgi                18     22  0
004500011122     D*-------------------
004600010731     D* COSTANTI
004700011122     D*-------------------
004800010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004900010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005000990908
005100091026
005200091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
005300091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
005400091026
005500010528
005600010528
005700010528
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000724     C*
006200010528     C                   exsr      opeini
006300010528     C                   exsr      rwvao
006400010528     C*
006500010528     C                   seton                                        lr
006600010528
006700010528
006800010528
006900010528
007000010528     C*--------------------------------------------------------
007100010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007200010528     C*--------------------------------------------------------
007300010528     C     PREELA        BEGSR
007400010528     C*
007500000724     C* SE OCCORRE SPEDIRE IN FILIALE
007600010528     C                   if        invfil <> *zeros and
007700010528     C                             flgGiro = '0'
007800010528     C*
007900010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008000010528     C                   eval      flgGiro = '1'
008100010529     C*
008200010529     C                   endif
008300010528     C*
008400010528     C                   ENDSR
008500010528     C***
008600010528
008700010528
008800010528     C*--------------------------------------------------------
008900010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009000010528     C*--------------------------------------------------------
009100010528     C     ENDELA        BEGSR
009200000613     C*
009300010528     C                   ENDSR
009400010528     C***
009500990908
009600010528
009700010528
009800910830     C*--------------------------------------------------------
009900020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
010000910830     C*--------------------------------------------------------
010100010122     C     RWVAO         BEGSR
010200010528     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600010122     C                   if        not %open(fnvaowwr)
010700010122     C                   open      fnvaowwr
010800990914     C                   endif
010900990910     C*
011000990910     C                   clear                   §CTROK
011100990910     C                   clear                   §CTRMO
011200990910     C                   clear                   §CTRNO
011300990910     C*
011400921023     C                   DO        *HIVAL
011500990913     C*
011600990915     C                   READ      tivin00r                               70
011700010731     C*
011800010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
011900010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
012000010618     C*
012100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012200010618     C                   if        vinflg = '1'
012300010618     C                   eval      flgOk = '1'
012400010618     C                   endif
012500010618     C*
012600000905     C                   if        vindta > *blanks
012700000613     C                   add       1             rrnum
012800990913     C*
012900010711     C                   if        *in70 = *off and
013000010711     C                             (vinflg = *blanks
013100010711     C                              or vinflg = '0'
013200010711     C                              or vinflg = '2')
013300990913     C*
013400010528     C                   clear                   FNVAO000
013500011105     C                   clear                   TIORI000
013600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013700010711     C                   if        vinflg = *blanks or vinflg = '0'
013800010711     C                   clear                   vinmsg
013900010711     C                   endif
014000020507     C*
014100071012     C* Solo se trattasi d record ORM
014200071012     C                   if        %subst(vindta:25:9) = '105239149'
014300020507     C                   exsr      inzvar
014400020527     C                   exsr      defcam
014500020507     C                   exsr      impvao
014600010122     C*
014700010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014800010528     C                   eval      depfil = VAOPOE
014900010528     C                   exsr      repfil
015000020507     C                   if        depfil = invfil
015100010528     C*
015200010528     C                   exsr      PREELA
015300010710     C*
015400930409     C*
015500010604     C  N31              ADD       1             §CTROK            7 0
015600010604     C   32              ADD       1             §CTRMO            7 0
015700010604     C   31              ADD       1             §CTRNO            7 0
015800020507     C*
015900030624     C  N31              EXSR      CHKCOR
016000011105     C  N31              EXSR      WRIORI
016100050826     C  N31              EXSR      WRIVGD
016200010122     C  N31              WRITE     FNVAO000
016300990910     C*
016400010604     C                   if        *in31 = *off and
016500010604     C                             *in32 = *off
016600990910     C                   eval      vinflg = '1'
016700990910     C                   else
016800990910     C                   eval      vinflg = '2'
016900991022     C                   endif
017000990910     C                   endif
017100010604     C*
017200010604     C                   endif
017300000905     C*
017400000905     C                   else
017500000905     C                   eval      vinflg = '1'
017600000905     C                   endif
017700071012     C*
017800071012     C                   endif
017900000905     C*
018000000905     C  N70              update    tivin000
018100991022     C*
018200991022     C  N70              ENDdo
018300010528     C*
018400010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018500010528     C                   if        cntNonEl = *zeros or
018600010528     C                             flgMulti = '0'
018700010528     C* Se non ci sono record con errori ...
018800010528     C                   if        §ctrno = 0 and
018900010604     C                             §ctrmo = 0 and
019000010528     C                             flgStato <> '2'
019100990910     C* ... restituisco esito OK.
019200990921     C                   eval      wrkesito = '0'
019300990910     C                   else
019400000710     C                   if        §ctrok > 0
019500990921     C                   eval      wrkesito = '1'
019600000710     C                   else
019700010615     C                   if        flgOk = '0'
019800010615     C                   eval      wrkesito = '2'
019900010615     C                   else
020000010615     C                   eval      wrkesito = '6'
020100010615     C                   endif
020200990910     C                   endif
020300010528     C                   endif
020400010529     C                   else
020500010529     C                   eval      wrkesito = '9'
020600000710     C                   endif
020700990910     C*
020800990914     C                   if        %open(tivin00r)
020900990908     C                   close     tivin00r
021000990914     C                   endif
021100010122     C                   if        %open(fnvaowwr)
021200010122     C                   close     fnvaowwr
021300990914     C                   endif
021400990910     C*
021500010528     C                   if        vlrpoi <> 999
021600010528     C                   eval      invfil = vlrpoi
021700010528     C                   endif
021800010528     C*
021900990920     C                   if        §ctrok > 0
022000010528     C                             and invfil > *zeros
022100000613     C                   exsr      invio
022200990920     C                   endif
022300010612     C*
022400010612     C                   if        flgGiro = '1'
022500010612     C                   exsr      endela
022600010612     C                   endif
022700010528     C*
022800910830     C                   ENDSR
022900000613     C***
023000990920
023100020507
023200020507     C*----------------------------------------------------*
023300020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023400020507     C*----------------------------------------------------*
023500020507     C     INZVAR        BEGSR
023600020507     C*
023700020507     C                   Z-ADD     *zeros        Num5_0            5 0
023800020507     C                   Z-ADD     *zeros        Num3_0            3 0
023900020507     C*
024000020507     C                   ENDSR
024100020507     C*----------------------------------------------------*
024200020507     C*  IMPOSTAZIONE CAMPI COSTANTI
024300020507     C*----------------------------------------------------*
024400020507     C     DEFCAM        BEGSR
024500020507     C*
024600020507     C* Imposto i valori di default...
024700071012 xxx C                   EVAL      VAOPOE = 005
024800020507     C                   EVAL      VAOTOR = 'S'
024900020507     C                   EVAL      VAOTCO = 'F'
025000100108     C                   EVAL      VAOCOR = 1301475000
025100100108     C                   EVAL      VAOCRC = 1301475000
025200020507     C                   EVAL      VAOFFD = *blanks
025300071015 xxx C                   EVAL      VAOPAG = 'O'
025400100108     C                   EVAL      VAOKSC = 1301475
025500071015 xxx C                   EVAL      VAODDT = 'N'
025600020507     C                   EVAL      VAOCTR = '000'
025700071015 xxx C****               EVAL      VAOPOC = 005
025800071016     C                   EVAL      VAOORR = 1600
025900020507     C* ... e poi verifico se sono stati passati come parametri
026000020507     C                   IF        vlrppt > *blanks
026100020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
026200020507     C                   EXSR      CHKNUM
026300020507     C                   IF        PiInt=*on
026400020507     C                   Z-ADD     PiVal         VAOCOR
026500020507     C                   Z-ADD     PiVal         VAOCRC
026600020507     C                   ENDIF
026700020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026800020507     C                   EXSR      CHKNUM
026900020507     C                   IF        PiInt=*on
027000020507     C                   Z-ADD     PiVal         VAOPOE
027100020507     C                   Z-ADD     PiVal         VAOPOC
027200020507     C                   ENDIF
027300020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
027400020507     C                   EXSR      CHKNUM
027500020507     C                   IF        PiInt=*on
027600020507     C                   Z-ADD     PiVal         Num3_0
027700020507     C                   MOVEL     NUM3_0        VAOCTR
027800020507     C                   ENDIF
027900020507     C                   ENDIF
028000020507     C*
028100020507     C                   ENDSR
028200020507     C*----------------------------------------------------*
028300020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
028400020507     C*----------------------------------------------------*
028500020507     C     IMPVAO        BEGSR
028600020507     C*
028700020507     C                   SETOFF                                       3132
028800020507     C*
028900020507     C* Reperimento campi ALFA
029000020507     C*
029100020507     C* Considerazioni sulla ragione sociale del destinatario da indicare
029200071012     C                   EVAL      VAORSR=%trim(%subst(vindta:34:44))
029300020507     C* == verifico se esiste il carattere @e, se c'è diventa A
029400020507     C     '@':'A'       XLATE     VAORSR        VAORSR
029500020507     C* ==
029600071012     C                   EVAL      VAOINR=%trim(%subst(vindta:144:160))
029700071012     C                   EVAL      VAOLOR=%trim(%subst(vindta:104:20))
029800071012     C                   EVAL      VAORER=%trim(%subst(vindta:351:45))
029900071012     C                   EVAL      VAOTER=%trim(%subst(vindta:396:20))
030000071012     C                   EVAL      VAORFA=%trim(%subst(vindta:1:24))
030100071012     C                   EVAL      VAONO1=%trim(%subst(vindta:315:36))
030200020507     C*
030300020507     C* Reperimento campi NUMERICI
030400020507     C*
030500020527     C* Data e ora apertura chiamata
030600020527     C                   Z-ADD     datcor        VAODAO
030700071016     C                   MOVEL     wn14          VAOOAO
030800020507     C*
030900020507     C* CAP ritiro
031000071012     C                   EVAL      PiStr=%trim(%subst(vindta:99:5))
031100020507     C                   EXSR      CHKNUM
031200020507     C                   IF        PiInt=*on
031300020507     C                   Z-ADD     PiVal         Num5_0
031400020507     C                   MOVEL     Num5_0        VAOCAR
031500020507     C                   ELSE
031600020507     C                   SETON                                        32
031700020507     C                   EVAL      VAOCAR = *blanks
031800020507     C                   EVAL      vinmsg = %trimr(vinmsg)
031900020507     C                             + ' ' + 'VAOCAR'
032000020507     C                   ENDIF
032100071016     C* Reperisco la provincia dal CAP e dalla località
032200071016     C                   IF        VAOCAR <> *blanks
032300071016     C                   CLEAR                   TISI95DS
032400071016     C                   EVAL      I95TCN = '3'
032500071016     C                   Z-ADD     datcor        I95DAT
032600071016     C                   EVAL      I95CAP = VAOCAR
032700071016     C                   EVAL      I95LOC = VAOLOR
032800071016     C                   CALL      'TISI95R'
032900071016     C                   PARM                    TISI95DS
033000071016     C                   EVAL      VAOPRR = O95PRV
033100071016     C                   ENDIF
033200020507     C*
033300020507     C* Data ritiro richiesta
033400071012     C                   EVAL      PiStr=%subst(vindta:459:4)+
033500071012     C                                   %subst(vindta:456:2)+
033600071012     C                                   %subst(vindta:453:2)
033700020507     C                   EXSR      CHKNUM
033800020507     C                   IF        PiInt=*on
033900020507     C                   Z-ADD     PiVal         VAODAR
034000020507     C                   ELSE
034100020507     C                   SETON                                        32
034200020507     C                   EVAL      VAODAR = *zeros
034300020507     C                   EVAL      vinmsg = %trimr(vinmsg)
034400020507     C                             + ' ' + 'VAODAR'
034500020507     C                   ENDIF
034600071012     C*
034700071012     C* Colli
034800071012     C                   EVAL      PiStr=%subst(vindta:484:4)
034900071012     C                   EXSR      CHKNUM
035000071012     C                   IF        PiInt=*on
035100071012     C                   Z-ADD     PiVal         VAONCL
035200071012     C                   ELSE
035300071012     C                   SETON                                        32
035400071012     C                   EVAL      VAONCL = *zeros
035500071012     C                   EVAL      vinmsg = %trimr(vinmsg)
035600071012     C                             + ' ' + 'VAONCL'
035700071012     C                   ENDIF
035800071012     C*
035900071012     C* Peso Kg
036000071012     C                   EVAL      PiStr=%subst(vindta:488:10)
036100071012     C                   EXSR      CHKNUM
036200071012     C                   IF        PiNum=*on
036300071012     C                   Z-ADD     PiVal         VAOPKG
036400071012     C                   ELSE
036500071012     C                   SETON                                        32
036600071012     C                   EVAL      VAOPKG = *zeros
036700071012     C                   EVAL      vinmsg = %trimr(vinmsg)
036800071012     C                             + ' ' + 'VAOPKG'
036900071012     C                   ENDIF
037000020507     C*
037100020507     C                   ENDSR
037200020507     C*----------------------------------------------------*
037300020507
037400020507
037500020507
037600020507     C*----------------------------------------------------*
037700020507     C*  CONTROLLO NUMERICITA' CAMPI
037800020507     C*----------------------------------------------------*
037900020507     C     CHKNUM        BEGSR
038000020507     C*
038100020507     C                   IF        PiDecChr = *blanks
038200020507     C                   EVAL      PiDecChr = '.'
038300020507     C                   ENDIF
038400020507     C*
038500020507     C                   CALL(e)   'ISNUMERIC'
038600020507     C                   PARM                    PiStr            30
038700020507     C                   PARM                    PiDecChr          1
038800020507     C                   PARM      *ZEROS        PiVal            30 9
038900020507     C                   PARM      '0'           PiInt             1
039000020507     C                   PARM      '0'           PiNum             1
039100020507     C                   IF        %error
039200020507     C                   EVAL      PiNum=*off
039300020507     C                   ENDIF
039400020507     C*
039500020507     C                   ENDSR
039600020507     C***
039700030624
039800030624
039900030624     C*------------------------------------------------------------------------*
040000030624     C* CHKCOR - Considerazioni su VAOCOR
040100030624     C*------------------------------------------------------------------------*
040200030624     C     CHKCOR        BEGSR
040300030624     C*
040400030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040500030624     C                   if        vaoTCO = 'F'
040600030624     C* Quindi verifico se il codice ordinante non è valorizzato
040700030625     C                   if        vaoCOR = *zeros  AND
040800030625     C                             vaoRSO = *blanks AND
040900030625     C                             vaoINO = *blanks AND
041000030625     C                             vaoCAO = *blanks AND
041100030625     C                             vaoLOO = *blanks AND
041200030625     C                             vaoPRO = *blanks AND
041300030625     C                             vaoNAO = *blanks
041400030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041500030624     C                   movel     '0'           wFlgAcr           1
041600030624     C                   move      vlrKSC        wCli              7 0
041700030624     C                   move(p)   wCli          acrCRO
041800030624     C     acrCRO        setll     fnacr01l
041900030624     C                   if        %found(fnacr01l)
042000030624     C                   read      fnacr01l
042100030624     C                   dow       not %eof(fnacr01l)
042200030624     C                   movel     acrCRO        wCliAcr           7 0
042300030624     C                   if        wCliAcr = wCli
042400030624     C                   eval      vaoCOR = acrCRO
042500030624     C                   movel     '1'           wFlgAcr
042600030624     C                   leave
042700030624     C                   else
042800030624     C                   leave
042900030624     C                   endif
043000030624     C                   read      fnacr01l
043100030624     C                   enddo
043200030624     C                   endif
043300030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
043400030624     C* dati anagrafici dell'ordinante dal piano dei conti
043500030624     C                   if        wFlgAcr = '0'
043600030624     C                   clear                   BS69DS
043700030624     C                   clear                   ACODS
043800030624     C                   clear                   INDDS
043900030624     C                   clear                   CLPDS
044000030624     C                   clear                   CLSDS
044100030624     C                   move(P)   vlrKSC        I69KAC
044200030624     C                   move(P)   vlrKSC        I69KIN
044300030624     C                   call      'TIBS69R'
044400030624     C                   parm                    BS69DS
044500030624     C                   parm                    ACODS
044600030624     C                   parm                    INDDS
044700030624     C                   parm                    CLPDS
044800030624     C                   parm                    CLSDS
044900030624     C     O69ERR        ifne      '1'
045000030624     C                   eval      vaoRSO = ACORAG
045100030624     C                   eval      vaoINO = INDVIA
045200030624     C                   movel(P)  INDCAP        vaoCAO
045300030624     C                   eval      vaoLOO = INDCIT
045400030624     C                   eval      vaoPRO = INDPRV
045500030624     C                   eval      vaoNAO = INDSTA
045600030624     C                   endif
045700030624     C                   endif
045800030624     C                   endif
045900030624     C                   endif
046000030624     C*
046100030624     C                   ENDSR
046200020507
046300020507
046400010528
046500011105     C*------------------------------------------------------------------------*
046600011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
046700011105     C*------------------------------------------------------------------------*
046800011105     C     WRIORI        BEGSR
046900011105     C*
047000090617     C* Come prima cosa stacco un numeratore da AZNUM
047100090617     C                   clear                   trul33ds
047200090617     C                   eval      I33OPE = *zeros
047300090617     C                   eval      I33CNU = 600
047400090617     C                   eval      I33NUM = 1
047500090617     C                   movel     TRUL33DS      KPJBU
047600090617     C                   call      'TRUL33R'
047700090617     C                   parm                    KPJBA
047800090617     C                   movel     KPJBU         TRUL33DS
047900090617     C                   if        O33ERR = *zeros
048000090617     C                   z-add     O33NRF        §numori
048100090618     c                   else
048200090618     c                   eval      §numori = *all'9'
048300090617     C                   endif
048400011105     C*
048500011122     C* Quindi imposto i campi "extra-VAO"
048600011122     C                   if        vlrKSC = '0ORM0000'
048700011122     C*
048800011122     C                   if        vaoCOR > *zeros
048900011122     C                   movel     vaoCOR        wrkCOR7           7 0
049000011122     C*
049100011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
049200011122     C* codice padre. Se è un padre assegno già il codice ordinante.
049300011122     C                   clear                   tibs10ds
049400011122     C                   eval      d10drf = datcor                              *data riferimento
049500011122     C                   eval      d10paf = 'P'                                 *cerca il padre
049600011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
049700091026     C*
049800091026     C* Preparazione chiamata al TIBS10R1
049900091026     C                   clear                   tibs10r1ds
050000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
050100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
050200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
050300091026     C*
050400091026     C                   call      'TIBS10R1'
050500091026     C                   parm                    tibs10r1ds
050600091026     C*
050700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050800011122     C*
050900011122     C                   if        d10err <> *blanks or                         *No unif. o padre
051000011122     C                             d10cop =  *zeros  or
051100011122     C                             d10cop =  d10cod
051200011122     C                   move      d10cod        oriIDC
051300011122     C                   else                                                   *Figlio
051400011122     C                   move      d10cop        oriIDC
051500011122     C                   endif
051600011122     C*
051700011122     C                   else
051800011122     C                   eval      oriIDC = *zeros
051900011122     C                   endif
052000011122     C*
052100011122     C                   else
052200011122     C                   eval      oriIDC = vlrKSC
052300011122     C                   endif
052400011122     C*
052500011105     C                   eval      oriPRG = §numori
052600011105     C                   eval      oriDER = datcor
052700011105     C                   eval      oriOER = wn6
052800011105     C                   eval      oriPOE = vaoPOE
052900011105     C                   eval      oriNSR = vaoNSR
053000011105     C                   eval      oriNOR = vaoNOR
053100011105     C                   eval      oriNRV = vaoNRV
053200011105     C                   eval      oriFLO = *blanks
053300011105     C*
053400011122     C* Infine valorizzo la chiave esterna sul file FNVAO
053500011105     C                   eval      dorm01 = vaoFLO
053600011105     C                   movel     §numori       §ormpg
053700011105     C                   eval      vaoFLO = dorm01
053800030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
053900030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
054000011105     C*
054100011105     C* ...quindi scrivo il file TIORI00F
054200011105     C                   WRITE     TIORI000
054300011105     C*
054400011105     C                   ENDSR
054500020507     C***
054600050826
054700050826
054800050826
054900050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
055000050826     C     wriVGD        BEGSR
055100050826     C*
055200050826     C* Reperisco la descrizione della fase da tabella
055300050826     C                   movel(p)  'FAR'         KteCOD
055400050826     C                   movel(p)  '000'         KteKE1
055500050826     C     KEYtbe        chain     tntbe01l
055600050826     C                   if        %found(tntbe01l)
055700050826     C                   movel(P)  tbeuni        DFAR
055800050826     C                   else
055900050826     C                   clear                   DFAR
056000050826     C                   endif
056100050826     C*
056200050826     C* Valorizzo buffer record
056300050826     C                   clear                   FNVAPDS
056400050826     C                   eval      vapIDC = oriIDC
056500050826     C                   eval      vapPOE = oriPOE
056600050826     C                   eval      vapRFA = vaoRFA
056700050826     C                   eval      vapPOG = oriPOE
056800050826     C                   eval      vapDAE = oriDER
056900050826     C                   eval      vapORE = oriOER
057000050826     C                   if        d§fardva <> *blanks
057100050826     C                   eval      vapDFA = d§fardva
057200050826     C                   else
057300050826     C                   eval      vapDFA = d§fardes
057400050826     C                   endif
057500050826     C*
057600050826     C                   clear                   tivgd000
057700080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057800050826     C                   eval      vgdTIP = 'VP'
057900050826     C                   eval      vgdKSU = vapIDC
058000050826     C                   eval      vgdTSC = 'WW'
058100050826     C                   eval      vgdDAT = datcor
058200071012     C                   eval      vgdPGM = 'TITV1B4R'
058300050826     C                   write     tivgd000
058400050826     C*
058500050826     C                   ENDSR
058600050826     C*------------------------------------------------------------------------*
058700011119
058800010528
058900010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059000010528     C     repfil        BEGSR
059100010528     C*
059200010528     C                   if        invfil = *zeros and
059300010528     C                             depfil > *zeros and
059400010613     C                             (vinflg = *blanks or
059500010613     C                              vinflg = *zeros)
059600010528     C
059700010528     C                   eval      invfil = depfil
059800010528     C                   endif
059900010528     C*
060000010529     C                   if        depfil <> invfil and
060100010529     C                             invfil > *zeros
060200010528     C                   eval      flgMulti = '1'
060300010528     C                   if        vinflg = *blanks
060400010528     C                   add       1             cntNonEl
060500010528     C                   endif
060600010528     C                   endif
060700010528     C*
060800010528     C                   if        vinflg = '2'
060900010528     C                   eval      flgStato = '2'
061000010528     C                   endif
061100010528     C*
061200010528     C                   ENDSR
061300010528     C***
061400010528
061500010528
061600010528
061700010528
061800990920      /TITLE Invio dei dati al punto operativo.
061900000613     C     invio         BEGSR
062000990920     C*
062100990920     C                   reset                   dscmz
062200010528     C                   move      invfil        cmzdst
062300990920     C                   eval      cmzfld = vlrfou
062400990920     C                   eval      cmzmbd = vlrhdl
062500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
062600000710     C                   if        prmfir = *blanks
062700010122     C                   eval      cmzfla = 'FNVAO00F'
062800010122     C                   eval      cmzmba = 'FNVAO00F'
062900000710     C                   else
063000000710     C                   eval      cmzfla = prmfir
063100000710     C                   eval      cmzmba = prmfir
063200000710     C                   endif
063300990920     C                   eval      cmznrr = *zeros
063400990920     C                   move      §ctrok        cmznrr
063500010123     C                   eval      cmzlba = vlrfl1
063600990920     C                   call(e)   'TIS711C'
063700990920     C                   parm                    dscmz
063800990921     C                   parm      *blanks       esito
063900990920     C                   if        %error
064000990920     C                             or cmzerr = '1'
064100990921     C                             or esito  = '1'
064200000710     C                   eval      wrkesito = '3'
064300990920     C                   endif
064400990920     C*
064500000613     C                   ENDSR
064600000613     C***
064700990910
064800010528
064900010528
065000010528
065100010528      /TITLE Invio dei dati al punto operativo.
065200010528     C     opeini        BEGSR
065300010528     C*
065400010528     C* Inizializzo flag e contatori operativi
065500010528     C                   movel     '0'           flgGiro           1
065600010528     C                   movel     '0'           flgMulti          1
065700010528     C                   movel     '1'           flgStato          1
065800010615     C                   movel     '0'           flgOk             1
065900010528     C                   z-add     *zeros        cntNonEl         10 0
066000010528     C                   z-add     *zeros        depfil            3 0
066100010528     C                   z-add     *zeros        invfil            3 0
066200010528     C*
066300010528     C                   ENDSR
066400010528     C***
066500010528
066600010528
066700010528
066800010528
066900000613     C     *inzsr        BEGSR
067000990910     C*
067100990910     C     *entry        plist
067200990920     C                   parm                    tivlrds
067300990921     C                   parm      wrkesito      esito
067400000724     C                   parm                    prmlit
067500000710     C                   parm                    prmfir
067600011105     C*
067700011105     C* Campi riferito al database
067800011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
067900011105     C     *like         define    tbeke1        kteKE1
068000011105     C*
068100011105     C* Definizione chiavi di lettura
068200011105     C* TNTBE01L - parziale
068300011105     C     keytbe        klist
068400011105     C                   kfld                    kteCOD                         *UTENTE
068500011105     C                   kfld                    kteKE1                         *CODICE
068600011105     C*
068700011105     C* Reperimento data e ora corrente
068800011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
068900011105     C                   movel     wn14          wn6               6 0          *ORA (6)
069000011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069100011105     C                   z-add     wn8           g08dat
069200011105     C                   z-add     *zeros        g08inv
069300011105     C                   movel     '0'           g08err
069400011105     C                   call      'XSRDA8'
069500011105     C                   parm                    wlbda8
069600011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069700010529     C*
069800000613     C                   ENDSR
069900000613     C***
