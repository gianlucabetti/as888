000100020527      /TITLE Upload via Internet: traduzione in FNVAOWWR. (Cliente S.G. Cod. 0632142)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500030624     Ffnacr01l  if   e           k disk
000600090618     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800010122     FFNVAOwwr  O    E             DISK    usropn
000900011105     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100020527     Ftitv14p   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300020527     Ftitv14ps  O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
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
002500000512     D* COMANDI
002600000512     D*------------
002700011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002800000512     D*------------
002900990920     D dscmz         e ds                  inz
003000990910     D psds           sds
003100990910     D  procname         *PROC
003200010122     D*
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400050826     D fnvapds       e ds                  extname(fnvap00f)
003500011105     D dorm01        e ds
003600011119     D dfar          e ds
003700090618     d trul33ds      e ds
003800090618     d kpjba         e ds
003900990910     D esito           s              1
004000000724     D prmlit          s             10
004100000710     D prmfir          s             10
004200990921     D wrkesito        s                   like(esito)
004300990915     D wrkdata         s               d
004400990915     D wrkora          s               t
004500000613     D rrnum           s              6  0 INZ(*zeros)
004600000621     D recko           s            150    INZ(*blanks)
004700011123     D depcmd          s            150    INZ(*blanks)
004800020507     D wrkRFA          s                   LIKE(vaorfa)
004900090618     d §numori         s              7  0 inz
005000011105     D*-------------------
005100011105     D* DS "XSRDA8" - controllo data (8)
005200011105     D*-------------------
005300011105     D wlbda8          DS                  INZ
005400011105     D  g08dat                 1      8  0
005500011105     D  g08inv                 9     16  0
005600011105     D  g08err                17     17
005700011105     D  g08tgi                18     22  0
005800011122     D*-------------------
005900010731     D* COSTANTI
006000011122     D*-------------------
006100010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006200010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006300990908
006400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006600010528
006700010528
006800010528
006900990915     C                   time                    wrkdata
007000990915     C                   time                    wrkora
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000724     C*
007500010528     C                   exsr      opeini
007600010528     C                   exsr      rwvao
007700010528     C*
007800010528     C                   seton                                        lr
007900010528
008000010528
008100010528
008200010528
008300010528     C*--------------------------------------------------------
008400010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010528     C*--------------------------------------------------------
008600010528     C     PREELA        BEGSR
008700010528     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900010528     C                   if        invfil <> *zeros and
009000010528     C                             flgGiro = '0'
009100010528     C*
009200010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010528     C                   eval      flgGiro = '1'
009400000724     C*
009500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700010529     C     invfil        chain     azorg01l
009800000724     C                   if        %found
009900000616     C                   movel(p)  CMD(1)        depcmd
010000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010100000616     C*
010200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010300011123     C                   Z-ADD     150           LENGH            15 5
010400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010500000616     C                   PARM                    depcmd
010600000616     C                   PARM                    LENGH
010700000724     C*
010800000724     C                   endif
010900000616     C*
011000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100000616     C                   movel(p)  CMD(2)        depcmd
011200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011300000616     C*
011400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011500011123     C                   Z-ADD     150           LENGH            15 5
011600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700000616     C                   PARM                    depcmd
011800000616     C                   PARM                    LENGH
011900000616     C*
012000020527     C                   if        not %open(titv14ps)
012100020527     C                   open      titv14ps
012200000616     C                   except    testdett
012300010528     C                   endif
012400010529     C*
012500010529     C                   endif
012600010528     C*
012700010528     C                   ENDSR
012800010528     C***
012900010528
013000010528
013100010528     C*--------------------------------------------------------
013200010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013300010528     C*--------------------------------------------------------
013400010528     C     ENDELA        BEGSR
013500000613     C*
013600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013700000613     C*
013800020527     C                   if        %open(titv14ps)
013900000616     C                   except    findett
014000020527     C                   close     titv14ps
014100000613     C                   endif
014200000616     C*
014300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014400011123     C                   Z-ADD     150           LENGH            15 5
014500010529     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    CMD(3)
014700000616     C                   PARM                    LENGH
014800000613     C*
014900010528     C                   ENDSR
015000010528     C***
015100010528
015200000613
015300000613
015400000613     C*--------------------------------------------------------
015500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015600000613     C*--------------------------------------------------------
015700000621     C     STPR          BEGSR
015800000613     C*
015900020527     C                   if        not %open(titv14p)
016000020527     C                   open      titv14p
016100990915     C                   endif
016200990915     C*
016300990915     C                   except    riepilogo
016400990915     C*
016500020527     C                   if        %open(titv14p)
016600020527     C                   close     titv14p
016700990914     C                   endif
016800990910     C*
016900000613     C                   ENDSR
017000000613     C***
017100990908
017200010528
017300010528
017400910830     C*--------------------------------------------------------
017500020507     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
017600910830     C*--------------------------------------------------------
017700010122     C     RWVAO         BEGSR
017800010528     C*
017900990914     C                   if        not %open(tivin00r)
018000990908     C                   open      tivin00r
018100990914     C                   endif
018200010122     C                   if        not %open(fnvaowwr)
018300010122     C                   open      fnvaowwr
018400990914     C                   endif
018500990910     C*
018600990910     C                   clear                   §CTROK
018700990910     C                   clear                   §CTRMO
018800990910     C                   clear                   §CTRNO
018900990910     C*
019000921023     C                   DO        *HIVAL
019100990913     C*
019200990915     C                   READ      tivin00r                               70
019300010731     C*
019400010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019500010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019600010618     C*
019700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019800010618     C                   if        vinflg = '1'
019900010618     C                   eval      flgOk = '1'
020000010618     C                   endif
020100010618     C*
020200000905     C                   if        vindta > *blanks
020300000613     C                   add       1             rrnum
020400990913     C*
020500010711     C                   if        *in70 = *off and
020600010711     C                             (vinflg = *blanks
020700010711     C                              or vinflg = '0'
020800010711     C                              or vinflg = '2')
020900990913     C*
021000010528     C                   clear                   FNVAO000
021100011105     C                   clear                   TIORI000
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600020507     C*
021700020507     C                   exsr      inzvar
021800020527     C                   exsr      defcam
021900020507     C                   exsr      impvao
022000010122     C*
022100010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010528     C                   eval      depfil = VAOPOE
022300010528     C                   exsr      repfil
022400020507     C                   if        depfil = invfil
022500010528     C*
022600010528     C                   exsr      PREELA
022700010710     C*
022800930409     C*
022900010604     C  N31              ADD       1             §CTROK            7 0
023000010604     C   32              ADD       1             §CTRMO            7 0
023100010604     C   31              ADD       1             §CTRNO            7 0
023200020507     C*
023300030624     C  N31              EXSR      CHKCOR
023400011105     C  N31              EXSR      WRIORI
023500050826     C  N31              EXSR      WRIVGD
023600010122     C  N31              WRITE     FNVAO000
023700990910     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000990910     C                   eval      vinflg = '1'
024100990910     C                   else
024200000621     C                   eval      recko = vindta
024300020527     C                   if        %open(titv14ps)
024400000616     C                   except    rigadett
024500010612     C                   endif
024600990910     C                   eval      vinflg = '2'
024700991022     C                   endif
024800990910     C                   endif
024900010604     C*
025000010604     C                   endif
025100000905     C*
025200000905     C                   else
025300000905     C                   eval      vinflg = '1'
025400000905     C                   endif
025500000905     C*
025600000905     C  N70              update    tivin000
025700991022     C*
025800991022     C  N70              ENDdo
025900010528     C*
026000010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026100010528     C                   if        cntNonEl = *zeros or
026200010528     C                             flgMulti = '0'
026300010528     C* Se non ci sono record con errori ...
026400010528     C                   if        §ctrno = 0 and
026500010604     C                             §ctrmo = 0 and
026600010528     C                             flgStato <> '2'
026700990910     C* ... restituisco esito OK.
026800990921     C                   eval      wrkesito = '0'
026900990910     C                   else
027000000710     C                   if        §ctrok > 0
027100990921     C                   eval      wrkesito = '1'
027200000710     C                   else
027300010615     C                   if        flgOk = '0'
027400010615     C                   eval      wrkesito = '2'
027500010615     C                   else
027600010615     C                   eval      wrkesito = '6'
027700010615     C                   endif
027800990910     C                   endif
027900010528     C                   endif
028000010529     C                   else
028100010529     C                   eval      wrkesito = '9'
028200000710     C                   endif
028300990910     C*
028400990914     C                   if        %open(tivin00r)
028500990908     C                   close     tivin00r
028600990914     C                   endif
028700010122     C                   if        %open(fnvaowwr)
028800010122     C                   close     fnvaowwr
028900990914     C                   endif
029000990910     C*
029100010528     C                   if        vlrpoi <> 999
029200010528     C                   eval      invfil = vlrpoi
029300010528     C                   endif
029400010528     C*
029500990920     C                   if        §ctrok > 0
029600010528     C                             and invfil > *zeros
029700000613     C                   exsr      invio
029800990920     C                   endif
029900010612     C*
030000010612     C                   if        flgGiro = '1'
030100010612     C                   exsr      endela
030200010612     C                   endif
030300010528     C*
030400910830     C                   ENDSR
030500000613     C***
030600990920
030700020507
030800020507     C*----------------------------------------------------*
030900020507     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031000020507     C*----------------------------------------------------*
031100020507     C     INZVAR        BEGSR
031200020507     C*
031300020507     C                   Z-ADD     *zeros        Num5_0            5 0
031400020507     C                   Z-ADD     *zeros        Num3_0            3 0
031500020507     C*
031600020507     C                   ENDSR
031700020507     C*----------------------------------------------------*
031800020507     C*  IMPOSTAZIONE CAMPI COSTANTI
031900020507     C*----------------------------------------------------*
032000020507     C     DEFCAM        BEGSR
032100020507     C*
032200020507     C* Imposto i valori di default...
032300020527     C                   EVAL      VAOPOE = 063
032400020507     C                   EVAL      VAOTOR = 'S'
032500020507     C                   EVAL      VAOTCO = 'F'
032600020527     C                   EVAL      VAOCOR = 0632142000
032700020527     C                   EVAL      VAOCRC = 0632142000
032800020507     C                   EVAL      VAOFFD = *blanks
032900020507     C                   EVAL      VAOPAG = 'D'
033000020527     C                   EVAL      VAOKSC = 0632142
033100020507     C                   EVAL      VAODDT = 'S'
033200020507     C                   EVAL      VAOCTR = '000'
033300020527     C                   EVAL      VAOPOC = 063
033400020507     C* ... e poi verifico se sono stati passati come parametri
033500020507     C                   IF        vlrppt > *blanks
033600020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:10))
033700020507     C                   EXSR      CHKNUM
033800020507     C                   IF        PiInt=*on
033900020507     C                   Z-ADD     PiVal         VAOCOR
034000020507     C                   Z-ADD     PiVal         VAOCRC
034100020507     C                   ENDIF
034200020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034300020507     C                   EXSR      CHKNUM
034400020507     C                   IF        PiInt=*on
034500020507     C                   Z-ADD     PiVal         VAOPOE
034600020507     C                   Z-ADD     PiVal         VAOPOC
034700020507     C                   ENDIF
034800020507     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
034900020507     C                   EXSR      CHKNUM
035000020507     C                   IF        PiInt=*on
035100020507     C                   Z-ADD     PiVal         Num3_0
035200020507     C                   MOVEL     NUM3_0        VAOCTR
035300020507     C                   ENDIF
035400020507     C                   ENDIF
035500020507     C*
035600020507     C                   ENDSR
035700020507     C*----------------------------------------------------*
035800020507     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
035900020507     C*----------------------------------------------------*
036000020507     C     IMPVAO        BEGSR
036100020507     C*
036200020507     C                   SETOFF                                       3132
036300020507     C*
036400020507     C* Reperimento campi ALFA
036500020507     C*
036600020507     C* Considerazioni sulla ragione sociale del destinatario da indicare
036700020527     C                   EVAL      VAORSR=%trim(%trim(%subst(vindta:09:20))+' '+
036800020527     C                                          %trim(%subst(vindta:29:20))+' '+
036900020527     C                                          %trim(%subst(vindta:49:40)))
037000020507     C* == verifico se esiste il carattere @e, se c'è diventa A
037100020507     C     '@':'A'       XLATE     VAORSR        VAORSR
037200020507     C* ==
037300020527     C                   EVAL      VAOINR=%trim(%subst(vindta:89:50))+' '+
037400020527     C                                    %trim(%subst(vindta:139:6))
037500020527     C                   EVAL      VAOLOR=%trim(%subst(vindta:185:50))
037600020527     C                   EVAL      VAOPRR=%trim(%subst(vindta:235:2))
037700020527     C                   EVAL      VAORER=%trim(%subst(vindta:387:50))
037800020527     C                   EVAL      VAOTER=%trim(%subst(vindta:247:14))
037900020507     C
038000020527     C                   EVAL      VAORFA=%trim(%subst(vindta:275:16))
038100020527     C                   EVAL      VAONO1=%trim(%subst(vindta:307:35))
038200020527     C                   EVAL      VAONO2=%trim(%subst(vindta:342:35))
038300020507     C*
038400020507     C* Reperimento campi NUMERICI
038500020507     C*
038600020527     C* Data e ora apertura chiamata
038700020527     C                   Z-ADD     datcor        VAODAO
038800020507     C*
038900020507     C* CAP ritiro
039000020527     C                   EVAL      PiStr=%trim(%subst(vindta:235:10))
039100020507     C                   EXSR      CHKNUM
039200020507     C                   IF        PiInt=*on
039300020507     C                   Z-ADD     PiVal         Num5_0
039400020507     C                   MOVEL     Num5_0        VAOCAR
039500020507     C                   ELSE
039600020507     C                   SETON                                        32
039700020507     C                   EVAL      VAOCAR = *blanks
039800020507     C                   EVAL      vinmsg = %trimr(vinmsg)
039900020507     C                             + ' ' + 'VAOCAR'
040000020507     C                   ENDIF
040100020507     C*
040200020507     C* Data ritiro richiesta
040300020527     C                   EVAL      PiStr=%subst(vindta:491:4)+
040400020527     C                                   %subst(vindta:489:2)+
040500020527     C                                   %subst(vindta:487:2)
040600020507     C                   EXSR      CHKNUM
040700020507     C                   IF        PiInt=*on
040800020507     C                   Z-ADD     PiVal         VAODAR
040900020507     C                   ELSE
041000020507     C                   SETON                                        32
041100020507     C                   EVAL      VAODAR = *zeros
041200020507     C                   EVAL      vinmsg = %trimr(vinmsg)
041300020507     C                             + ' ' + 'VAODAR'
041400020507     C                   ENDIF
041500020507     C*
041600020507     C* Ora ritiro richiesta
041700020527     C                   EVAL      PiStr=%subst(vindta:495:2)+
041800020527     C                                   %subst(vindta:498:2)
041900020507     C                   EXSR      CHKNUM
042000020507     C                   IF        PiInt=*on
042100020507     C                   Z-ADD     PiVal         VAOORR
042200020507     C                   ELSE
042300020507     C                   SETON                                        32
042400020507     C                   EVAL      VAOORR = *zeros
042500020507     C                   EVAL      vinmsg = %trimr(vinmsg)
042600020507     C                             + ' ' + 'VAOORR'
042700020507     C                   ENDIF
042800020507     C*
042900020507     C                   ENDSR
043000020507     C*----------------------------------------------------*
043100020507
043200020507
043300020507
043400020507     C*----------------------------------------------------*
043500020507     C*  CONTROLLO NUMERICITA' CAMPI
043600020507     C*----------------------------------------------------*
043700020507     C     CHKNUM        BEGSR
043800020507     C*
043900020507     C                   IF        PiDecChr = *blanks
044000020507     C                   EVAL      PiDecChr = '.'
044100020507     C                   ENDIF
044200020507     C*
044300020507     C                   CALL(e)   'ISNUMERIC'
044400020507     C                   PARM                    PiStr            30
044500020507     C                   PARM                    PiDecChr          1
044600020507     C                   PARM      *ZEROS        PiVal            30 9
044700020507     C                   PARM      '0'           PiInt             1
044800020507     C                   PARM      '0'           PiNum             1
044900020507     C                   IF        %error
045000020507     C                   EVAL      PiNum=*off
045100020507     C                   ENDIF
045200020507     C*
045300020507     C                   ENDSR
045400020507     C***
045500030624
045600030624
045700030624     C*------------------------------------------------------------------------*
045800030624     C* CHKCOR - Considerazioni su VAOCOR
045900030624     C*------------------------------------------------------------------------*
046000030624     C     CHKCOR        BEGSR
046100030624     C*
046200030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
046300030624     C                   if        vaoTCO = 'F'
046400030624     C* Quindi verifico se il codice ordinante non è valorizzato
046500030625     C                   if        vaoCOR = *zeros  AND
046600030625     C                             vaoRSO = *blanks AND
046700030625     C                             vaoINO = *blanks AND
046800030625     C                             vaoCAO = *blanks AND
046900030625     C                             vaoLOO = *blanks AND
047000030625     C                             vaoPRO = *blanks AND
047100030625     C                             vaoNAO = *blanks
047200030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
047300030624     C                   movel     '0'           wFlgAcr           1
047400030624     C                   move      vlrKSC        wCli              7 0
047500030624     C                   move(p)   wCli          acrCRO
047600030624     C     acrCRO        setll     fnacr01l
047700030624     C                   if        %found(fnacr01l)
047800030624     C                   read      fnacr01l
047900030624     C                   dow       not %eof(fnacr01l)
048000030624     C                   movel     acrCRO        wCliAcr           7 0
048100030624     C                   if        wCliAcr = wCli
048200030624     C                   eval      vaoCOR = acrCRO
048300030624     C                   movel     '1'           wFlgAcr
048400030624     C                   leave
048500030624     C                   else
048600030624     C                   leave
048700030624     C                   endif
048800030624     C                   read      fnacr01l
048900030624     C                   enddo
049000030624     C                   endif
049100030624     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
049200030624     C* dati anagrafici dell'ordinante dal piano dei conti
049300030624     C                   if        wFlgAcr = '0'
049400030624     C                   clear                   BS69DS
049500030624     C                   clear                   ACODS
049600030624     C                   clear                   INDDS
049700030624     C                   clear                   CLPDS
049800030624     C                   clear                   CLSDS
049900030624     C                   move(P)   vlrKSC        I69KAC
050000030624     C                   move(P)   vlrKSC        I69KIN
050100030624     C                   call      'TIBS69R'
050200030624     C                   parm                    BS69DS
050300030624     C                   parm                    ACODS
050400030624     C                   parm                    INDDS
050500030624     C                   parm                    CLPDS
050600030624     C                   parm                    CLSDS
050700030624     C     O69ERR        ifne      '1'
050800030624     C                   eval      vaoRSO = ACORAG
050900030624     C                   eval      vaoINO = INDVIA
051000030624     C                   movel(P)  INDCAP        vaoCAO
051100030624     C                   eval      vaoLOO = INDCIT
051200030624     C                   eval      vaoPRO = INDPRV
051300030624     C                   eval      vaoNAO = INDSTA
051400030624     C                   endif
051500030624     C                   endif
051600030624     C                   endif
051700030624     C                   endif
051800030624     C*
051900030624     C                   ENDSR
052000020507
052100020507
052200010528
052300011105     C*------------------------------------------------------------------------*
052400011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
052500011105     C*------------------------------------------------------------------------*
052600011105     C     WRIORI        BEGSR
052700011105     C*
052800090618     C* Come prima cosa stacco un numeratore da AZNUM
052900090618     C                   clear                   trul33ds
053000090618     C                   eval      I33OPE = *zeros
053100090618     C                   eval      I33CNU = 600
053200090618     C                   eval      I33NUM = 1
053300090618     C                   movel     TRUL33DS      KPJBU
053400090618     C                   call      'TRUL33R'
053500090618     C                   parm                    KPJBA
053600090618     C                   movel     KPJBU         TRUL33DS
053700090618     C                   if        O33ERR = *zeros
053800090618     C                   z-add     O33NRF        §numori
053900090618     c                   else
054000090618     c                   eval      §numori = *all'9'
054100090618     C                   endif
054200011105     C*
054300011122     C* Quindi imposto i campi "extra-VAO"
054400011122     C                   if        vlrKSC = '0ORM0000'
054500011122     C*
054600011122     C                   if        vaoCOR > *zeros
054700011122     C                   movel     vaoCOR        wrkCOR7           7 0
054800011122     C*
054900011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
055000011122     C* codice padre. Se è un padre assegno già il codice ordinante.
055100011122     C                   clear                   tibs10ds
055200011122     C                   eval      d10drf = datcor                              *data riferimento
055300011122     C                   eval      d10paf = 'P'                                 *cerca il padre
055400011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
055500091026     C*
055600091026     C* Preparazione chiamata al TIBS10R1
055700091026     C                   clear                   tibs10r1ds
055800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
055900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
056000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
056100091026     C*
056200091026     C                   call      'TIBS10R1'
056300091026     C                   parm                    tibs10r1ds
056400091026     C*
056500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
056600011122     C*
056700011122     C                   if        d10err <> *blanks or                         *No unif. o padre
056800011122     C                             d10cop =  *zeros  or
056900011122     C                             d10cop =  d10cod
057000011122     C                   move      d10cod        oriIDC
057100011122     C                   else                                                   *Figlio
057200011122     C                   move      d10cop        oriIDC
057300011122     C                   endif
057400011122     C*
057500011122     C                   else
057600011122     C                   eval      oriIDC = *zeros
057700011122     C                   endif
057800011122     C*
057900011122     C                   else
058000011122     C                   eval      oriIDC = vlrKSC
058100011122     C                   endif
058200011122     C*
058300011105     C                   eval      oriPRG = §numori
058400011105     C                   eval      oriDER = datcor
058500011105     C                   eval      oriOER = wn6
058600011105     C                   eval      oriPOE = vaoPOE
058700011105     C                   eval      oriNSR = vaoNSR
058800011105     C                   eval      oriNOR = vaoNOR
058900011105     C                   eval      oriNRV = vaoNRV
059000011105     C                   eval      oriFLO = *blanks
059100011105     C*
059200011122     C* Infine valorizzo la chiave esterna sul file FNVAO
059300011105     C                   eval      dorm01 = vaoFLO
059400011105     C                   movel     §numori       §ormpg
059500011105     C                   eval      vaoFLO = dorm01
059600030603     C                   eval      %subst(VAOFLO:5:1) = *blanks
059700030603     C                   eval      %subst(VAOFLO:6:1) = *blanks
059800011105     C*
059900011105     C* ...quindi scrivo il file TIORI00F
060000011105     C                   WRITE     TIORI000
060100011105     C*
060200011105     C                   ENDSR
060300020507     C***
060400050826
060500050826
060600050826
060700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
060800050826     C     wriVGD        BEGSR
060900050826     C*
061000050826     C* Reperisco la descrizione della fase da tabella
061100050826     C                   movel(p)  'FAR'         KteCOD
061200050826     C                   movel(p)  '000'         KteKE1
061300050826     C     KEYtbe        chain     tntbe01l
061400050826     C                   if        %found(tntbe01l)
061500050826     C                   movel(P)  tbeuni        DFAR
061600050826     C                   else
061700050826     C                   clear                   DFAR
061800050826     C                   endif
061900050826     C*
062000050826     C* Valorizzo buffer record
062100050826     C                   clear                   FNVAPDS
062200050826     C                   eval      vapIDC = oriIDC
062300050826     C                   eval      vapPOE = oriPOE
062400050826     C                   eval      vapRFA = vaoRFA
062500050826     C                   eval      vapPOG = oriPOE
062600050826     C                   eval      vapDAE = oriDER
062700050826     C                   eval      vapORE = oriOER
062800050826     C                   if        d§fardva <> *blanks
062900050826     C                   eval      vapDFA = d§fardva
063000050826     C                   else
063100050826     C                   eval      vapDFA = d§fardes
063200050826     C                   endif
063300050826     C*
063400050826     C                   clear                   tivgd000
063500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
063600050826     C                   eval      vgdTIP = 'VP'
063700050826     C                   eval      vgdKSU = vapIDC
063800050826     C                   eval      vgdTSC = 'WW'
063900050826     C                   eval      vgdDAT = datcor
064000050826     C                   eval      vgdPGM = 'TITV14R'
064100050826     C                   write     tivgd000
064200050826     C*
064300050826     C                   ENDSR
064400050826     C*------------------------------------------------------------------------*
064500011119
064600010528
064700010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064800010528     C     repfil        BEGSR
064900010528     C*
065000010528     C                   if        invfil = *zeros and
065100010528     C                             depfil > *zeros and
065200010613     C                             (vinflg = *blanks or
065300010613     C                              vinflg = *zeros)
065400010528     C
065500010528     C                   eval      invfil = depfil
065600010528     C                   endif
065700010528     C*
065800010529     C                   if        depfil <> invfil and
065900010529     C                             invfil > *zeros
066000010528     C                   eval      flgMulti = '1'
066100010528     C                   if        vinflg = *blanks
066200010528     C                   add       1             cntNonEl
066300010528     C                   endif
066400010528     C                   endif
066500010528     C*
066600010528     C                   if        vinflg = '2'
066700010528     C                   eval      flgStato = '2'
066800010528     C                   endif
066900010528     C*
067000010528     C                   ENDSR
067100010528     C***
067200010528
067300010528
067400010528
067500010528
067600990920      /TITLE Invio dei dati al punto operativo.
067700000613     C     invio         BEGSR
067800990920     C*
067900990920     C                   reset                   dscmz
068000010528     C                   move      invfil        cmzdst
068100990920     C                   eval      cmzfld = vlrfou
068200990920     C                   eval      cmzmbd = vlrhdl
068300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
068400000710     C                   if        prmfir = *blanks
068500010122     C                   eval      cmzfla = 'FNVAO00F'
068600010122     C                   eval      cmzmba = 'FNVAO00F'
068700000710     C                   else
068800000710     C                   eval      cmzfla = prmfir
068900000710     C                   eval      cmzmba = prmfir
069000000710     C                   endif
069100990920     C                   eval      cmznrr = *zeros
069200990920     C                   move      §ctrok        cmznrr
069300010123     C                   eval      cmzlba = vlrfl1
069400990920     C                   call(e)   'TIS711C'
069500990920     C                   parm                    dscmz
069600990921     C                   parm      *blanks       esito
069700990920     C                   if        %error
069800990920     C                             or cmzerr = '1'
069900990921     C                             or esito  = '1'
070000000710     C                   eval      wrkesito = '3'
070100990920     C                   endif
070200990920     C*
070300000613     C                   ENDSR
070400000613     C***
070500990910
070600010528
070700010528
070800010528
070900010528      /TITLE Invio dei dati al punto operativo.
071000010528     C     opeini        BEGSR
071100010528     C*
071200010528     C* Inizializzo flag e contatori operativi
071300010528     C                   movel     '0'           flgGiro           1
071400010528     C                   movel     '0'           flgMulti          1
071500010528     C                   movel     '1'           flgStato          1
071600010615     C                   movel     '0'           flgOk             1
071700010528     C                   z-add     *zeros        cntNonEl         10 0
071800010528     C                   z-add     *zeros        depfil            3 0
071900010528     C                   z-add     *zeros        invfil            3 0
072000010528     C*
072100010528     C                   ENDSR
072200010528     C***
072300010528
072400010528
072500010528
072600010528
072700000613     C     *inzsr        BEGSR
072800990910     C*
072900990910     C     *entry        plist
073000990920     C                   parm                    tivlrds
073100990921     C                   parm      wrkesito      esito
073200000724     C                   parm                    prmlit
073300000710     C                   parm                    prmfir
073400011105     C*
073500011105     C* Campi riferito al database
073600011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
073700011105     C     *like         define    tbeke1        kteKE1
073800011105     C*
073900011105     C* Definizione chiavi di lettura
074000011105     C* TNTBE01L - parziale
074100011105     C     keytbe        klist
074200011105     C                   kfld                    kteCOD                         *UTENTE
074300011105     C                   kfld                    kteKE1                         *CODICE
074400011105     C*
074500011105     C* Reperimento data e ora corrente
074600011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
074700011105     C                   movel     wn14          wn6               6 0          *ORA (6)
074800011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074900011105     C                   z-add     wn8           g08dat
075000011105     C                   z-add     *zeros        g08inv
075100011105     C                   movel     '0'           g08err
075200011105     C                   call      'XSRDA8'
075300011105     C                   parm                    wlbda8
075400011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075500010529     C*
075600000613     C                   ENDSR
075700000613     C***
075800990908
075900020527     Otitv14p   E            riepilogo         2
076000990915     O                                              'Upload via Internet'
076100990915     O                                           +1 'Traduzione TIVIN00R -'
076200010122     O                                           +1 'FNVAOWWR'
076300010122     O                                           +1 'Ordini ritiro merce'
076400990915     O          E            riepilogo   2
076500990915     O                       wrkdata
076600990915     O                       wrkora              +1
076700990915     O                       procname            +1
076800990915     O          E            riepilogo   2
076900990915     O                                              'Cliente..................:'
077000010122     O                       VAOKSC        z     +1
077100990915     O          E            riepilogo   2
077200990920     O                                              'Riferimento Strategi.....:'
077300990920     O                       vlrhdl              +1
077400990915     O          E            riepilogo   2
077500990915     O                                              'Giusti...................:'
077600971022     O                       §CTROK        2   +  1
077700990915     O          E            riepilogo   2
077800990915     O                                              'Sbagliati e corretti.....:'
077900971022     O                       §CTRMO        2   +  1
078000990915     O          E            riepilogo   2
078100990915     O                                              'Sbagliati e scartati.....:'
078200971022     O                       §CTRNO        2   +  1
078300000613
078400020527     Otitv14ps  E            testdett          2
078500000613     O                                              'Upload via Internet'
078600000613     O                                           +1 'Traduzione TIVIN00R -'
078700010122     O                                           +1 'FNVAOWWR'
078800010122     O                                           +1 'Ordini ritiro merce'
078900000616     O          E            testdett    3
079000000613     O                                           +2 'N° rec'
079100000613     O                                           +3 'Anteprima contenuto'
079200000616     O          E            rigadett    2
079300000613     O                       rrnum               +2
079400000621     O                       recko               +3
079500000616     O          E            findett     2
079600000613     O                       wrkdata
079700000613     O                       wrkora              +1
079800000613     O                       procname            +1
079900000616     O          E            findett     2
080000000613     O                                              'Cliente..................:'
080100010122     O                       VAOKSC        z     +1
080200000616     O          E            findett     2
080300000613     O                                              'Riferimento Strategi.....:'
080400000613     O                       vlrhdl              +1
080500000616     O          E            findett     2
080600000613     O                                              'Giusti...................:'
080700000613     O                       §CTROK        2   +  1
080800000616     O          E            findett     2
080900000613     O                                              'Sbagliati e corretti.....:'
081000000613     O                       §CTRMO        2   +  1
081100000616     O          E            findett     2
081200000613     O                                              'Sbagliati e scartati.....:'
081300000613     O                       §CTRNO        2   +  1
081400000512** CMD - COMANDI CL
081500020527OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV14P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
081600020527OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV14PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
081700020527DLTOVR FILE(TITV14P TITV14PS) LVL(*)
081800000512
081900000512
