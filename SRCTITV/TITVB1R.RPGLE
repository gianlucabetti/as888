000100060626      /TITLE Upload via Internet: traduzione in FIVABWWR. (x STANHOME Cod. 0632875)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700060626     FFIVATwwr  O    E             DISK    usropn
000800040405     Ftitvb1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040405     Ftitvb1ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300060626     D parccm          s              8    INZ(*blanks)
003400060626     D parmbr          s             10    INZ(*blanks)
003500060626     D paropz          s              1    INZ(*blanks)
003600060626     D chkcall         s              1    INZ(*blanks)
003700010330
003800010330
003900010330
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010601     C*
004600010601     C                   exsr      opeini
004700010605     C                   exsr      rwvab
004800060626     C*
004900060626     C* Effettuo la chiamata al CLLE preposto
005000060626     C                   call(e)   'TITVVTC'
005100060626     C                   parm                    parccm
005200060626     C                   parm                    parmbr
005300060626     C                   parm      '2'           paropz
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400010601     C     invfil        chain     azorg01l
007500000724     C                   if        %found
007600000724     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000724     C*
007900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000724     C                   PARM                    depcmd
008300000724     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000613     C*
009700040405     C                   if        not %open(titvb1ps)
009800040405     C                   open      titvb1ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010601     C*--------------------------------------------------------
011200010601     C     ENDELA        BEGSR
011300990915     C*
011400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500000613     C*
011600040405     C                   if        %open(titvb1ps)
011700000616     C                   except    findett
011800040405     C                   close     titvb1ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700040405     C                   if        not %open(titvb1p)
013800040405     C                   open      titvb1p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300040405     C                   if        %open(titvb1p)
014400040405     C                   close     titvb1p
014500990914     C                   endif
014600000613     C*
014700000613     C                   ENDSR
014800000613     C***
014900000613
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000030130     C                   if        not %open(fivabwwr)
016100030130     C                   open      fivabwwr
016200010330     C                   endif
016300060626     C*
016400060626     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500060626     C                   exsr      prevat
016600060626     C*
016700060626     C                   if        chkcall = '0'
016800060626     C*
016900060626     C                   if        not %open(fivatwwr)
017000060626     C                   open      fivatwwr
017100060626     C                   endif
017200010330     C*
017300060626     C                   clear                   §CTROKVB          5 0
017400060626     C                   clear                   §CTROKVT          5 0
017500010604     C                   clear                   §CTRMO
017600010604     C                   clear                   §CTRNO
017700010330     C*
017800010330     C                   DO        *HIVAL
017900010330     C*
018000010330     C                   READ      tivin00r                               70
018100010618     C*
018200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018300010618     C                   if        vinflg = '1'
018400010618     C                   eval      flgOk = '1'
018500010618     C                   endif
018600010618     C*
018700010330     C                   if        vindta > *blanks
018800010330     C                   add       1             rrnum
018900010330     C*
019000010601     C                   if        *in70 = *off and
019100010330     C                             (vinflg = *blanks
019200010330     C                              or vinflg = '0'
019300010330     C                              or vinflg = '2')
019400010330     C*
019500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019600010711     C                   if        vinflg = *blanks or vinflg = '0'
019700010711     C                   clear                   vinmsg
019800010711     C                   endif
019900010601     C*
020000010330     C                   exsr      inzvar
020100010330     C                   exsr      defcam
020200101201     C                   if        (datcor >= 20110103 and wNEW = 'S') or
020300101201     C                             wNEW = 'T'
020400101201     C                   exsr      impvab2
020500101129     C                   else
020600101201     C                   exsr      impvab
020700101129     C                   endif
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010605     C                   eval      depfil = VABLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300030130     C                   if        vlrpoi = 999
021400030130     C                   MOVE(P)   invfil        VABFGS
021500030130     C                   else
021600030130     C                   MOVE(P)   vlrpoi        VABFGS
021700030130     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300060626     C  N31              ADD       1             §CTROKVB
022400010604     C   32              ADD       1             §CTRMO            7 0
022500010604     C   31              ADD       1             §CTRNO            7 0
022600020205     C*
022700101201     C                   if        (datcor >= 20110103 and wNEW = 'S') or
022800101201     C                             wNEW = 'T'
022900101201     C  N31              exsr      wrivat2                                      => carico/scrivo VAT
023000101129     C                   else
023100101201     C  N31              exsr      wrivat                                       => carico/scrivo VAT
023200101129     C                   endif
023300101216     C*
023400101216     C* Forzatura data consegna richiesta x traffico 2551153
023500101216     C                   if        VABCCM = 2551153
023600101216     C                   select
023700101216     C                   when      datcor  = 20101221
023800101216     C                   eval      VABDCR  = 20101224
023900101216     C                   eval      VABTCR  = 'P'
024000101221     C***                when      datcor >= 20101222 and
024100101221     C***                          datcor <= 20101229
024200101221     C***                eval      VABDCR  = 20110101
024300101221     C***                eval      VABTCR  = 'P'
024400101216     C                   endsl
024500101216     C                   endif
024600101216     C*
024700030130     C  N31              WRITE     FIVAB000
024800020722     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      recko = vindta
025400010604     C                   except    rigadett
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700010330     C  N70              ENDdo
026800060626     C*
026900060626     C                   endif                                                  <= end PREVAT
027000010601     C*
027100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027200010601     C                   if        cntNonEl = *zeros or
027300010601     C                             flgMulti = '0'
027400010330     C* Se non ci sono record con errori ...
027500010601     C                   if        §ctrno = 0 and
027600010604     C                             §ctrmo = 0 and
027700010601     C                             flgStato <> '2'
027800010330     C* ... restituisco esito OK.
027900010330     C                   eval      wrkesito = '0'
028000010330     C                   else
028100060626     C                   if        §ctrokvb > 0
028200010330     C                   eval      wrkesito = '1'
028300010330     C                   else
028400010615     C                   if        flgOk = '0'
028500010615     C                   eval      wrkesito = '2'
028600010615     C                   else
028700010615     C                   eval      wrkesito = '6'
028800010615     C                   endif
028900010330     C                   endif
029000010330     C                   endif
029100010601     C                   else
029200010601     C                   eval      wrkesito = '9'
029300010601     C                   endif
029400010330     C*
029500010330     C                   if        %open(tivin00r)
029600010330     C                   close     tivin00r
029700010330     C                   endif
029800030130     C                   if        %open(fivabwwr)
029900030130     C                   close     fivabwwr
030000010330     C                   endif
030100060626     C                   if        %open(fivatwwr)
030200060626     C                   close     fivatwwr
030300060626     C                   endif
030400010601     C*
030500010601     C                   if        vlrpoi <> 999
030600010601     C                   eval      invfil = vlrpoi
030700010601     C                   endif
030800010330     C*
030900060626     C                   if        §ctrokvb > 0
031000010601     C                             and invfil > *zeros
031100010330     C                   exsr      invio
031200010330     C                   endif
031300010601     C*
031400010618     C                   if        flgGiro = '1'
031500010601     C                   exsr      endela
031600010618     C                   endif
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000010330
032100010601
032200060626     C*----------------------------------------------------*
032300060626     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
032400060626     C*----------------------------------------------------*
032500060626     C     WRIVAT        BEGSR
032600070314     C*
032700070314     C                   eval      VATAAS = VABAAS
032800070314     C                   eval      VATLNP = VABLNP
032900070314     C                   eval      VATNRS = VABNRS
033000070314     C                   eval      VATNSP = VABNSP
033100070314     C                   eval      VATCCM = VABCCM
033200060626     C*
033300060626     C* Se richiesta gestione Disk C nei parametri
033400060626     C                   if        wDISK = 'C'
033500060626     C*
033600060626     C* X ogni collo indicato in testata
033700060626     C                   z-add     1             wColli            5 0
033800060626     C                   dow       wColli <= VABNCL
033900060626     C*
034000060626     C                   eval      VATTRC = 'E'
034100060626     C*
034200060626     C* Valorizzo il buffer di scrittura del FIVAT
034300080923     C                   move(p)   wColli        wColliAlfa        3
034400060626     C                   eval      VATNOT = %subst(vindta:79:7)+
034500060626     C                                      wColliAlfa
034600060626     C                   write     FIVAT000
034700060626     C*
034800060626     C                   add       1             wColli
034900060626     C                   enddo
035000060626     C*
035100060626     C                   endif
035200070314     C*
035300070314     C* Imposto il tipo record B x il numero telefono destinatario
035400070314     C                   eval      VATTRC = 'B'
035500070314     C                   eval      VATNOT = %trim(%subst(vindta:147:5))+' '+
035600100714     C                                      %trim(%subst(vindta:152:9))
035700070314     C                   if        VATNOT <> *blanks
035800070314     C                   write     FIVAT000
035900070314     C                   endif
036000060626     C*
036100060626     C                   ENDSR
036200101129     C*----------------------------------------------------*
036300101129     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
036400101129     C*----------------------------------------------------*
036500101129     C     WRIVAT2       BEGSR
036600101129     C*
036700101129     C                   eval      VATAAS = VABAAS
036800101129     C                   eval      VATLNP = VABLNP
036900101129     C                   eval      VATNRS = VABNRS
037000101129     C                   eval      VATNSP = VABNSP
037100101129     C                   eval      VATCCM = VABCCM
037200101129     C*
037300101129     C* Se richiesta gestione Disk C nei parametri
037400101129     C                   if        wDISK = 'C'
037500101129     C*
037600101129     C* X ogni collo indicato in testata
037700101129     C                   z-add     1             wColli            5 0
037800101129     C                   dow       wColli <= VABNCL
037900101129     C*
038000101129     C                   eval      VATTRC = 'E'
038100101129     C*
038200101129     C* Valorizzo il buffer di scrittura del FIVAT
038300101129     C                   move(p)   wColli        wColliAlfa        3
038400101129     C                   eval      VATNOT = %subst(vindta:123:7)+
038500101129     C                                      wColliAlfa
038600101129     C                   write     FIVAT000
038700101129     C*
038800101129     C                   add       1             wColli
038900101129     C                   enddo
039000101129     C*
039100101129     C                   endif
039200101129     C*
039300101129     C* Imposto il tipo record B x il numero telefono destinatario
039400101129     C                   eval      VATTRC = 'B'
039500101129     C                   if        %subst(vindta:206:15) <> *blanks
039600101129     C                   eval      VATNOT = %trim(%subst(vindta:206:5))+' '+
039700101129     C                                      %trim(%subst(vindta:211:9))
039800101129     C                   else
039900101129     C                   eval      VATNOT = %trim(%subst(vindta:191:5))+' '+
040000101201     C                                      %trim(%subst(vindta:196:9))
040100101129     C                   endif
040200101129     C*
040300101129     C                   if        VATNOT <> *blanks
040400101129     C                   write     FIVAT000
040500101129     C                   endif
040600101129     C*
040700101129     C                   ENDSR
040800101129
040900101129
041000101129
041100060626     C*----------------------------------------------------*
041200060626     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041300060626     C*----------------------------------------------------*
041400060626     C     PREVAT        BEGSR
041500060626     C*
041600060626     C* Compongo il nome del membro da dare al FIVATWWR
041700060626     C                   eval      parmbr = vlrhdl
041800060626     C                   movel     'M'           parmbr
041900060626     C                   eval      parccm = vlrksc
042000060626     C                   eval      paropz = '1'
042100060626     C* Effettuo la chiamata al CLLE preposto
042200060626     C                   call(e)   'TITVVTC'
042300060626     C                   parm                    parccm
042400060626     C                   parm                    parmbr
042500060626     C                   parm                    paropz
042600060626     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042700060626     C                   if        %error
042800060626     C                   movel     '1'           chkcall
042900060626     C                   else
043000060626     C                   movel     '0'           chkcall
043100060626     C                   endif
043200060626     C*
043300060626     C                   ENDSR
043400010601
043500010601
043600010601
043700010330     C*----------------------------------------------------*
043800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043900010330     C*----------------------------------------------------*
044000010330     C     INZVAR        BEGSR
044100010330     C*
044200020204     C                   Z-ADD     *zeros        Num5_0            5 0
044300060626     C                   Z-ADD     *zeros        Num7_0            7 0
044400010330     C*
044500010330     C                   ENDSR
044600010330     C*----------------------------------------------------*
044700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
044800010330     C*----------------------------------------------------*
044900010330     C     DEFCAM        BEGSR
045000010330     C*
045100020204     C* Inizializzo il buffer del record da scrivere
045200030130     C                   CLEAR                   FIVAB000
045300020204     C* Imposto i valori di default...
045400060626     C                   EVAL      VABCCM = 0632875
045500060626     C                   EVAL      VATCCM = 0632875
045600060626     C                   EVAL      VABLNP = 063
045700060626     C                   EVAL      VATLNP = 063
045800060626     C                   EVAL      VABCTR = 001
045900060626     C                   Z-ADD     vlrpoi        VABFGS
046000060626     C                   Z-ADD     vlrpoi        VATFGS
046100020204     C* ... e poi verifico se sono stati passati come parametri
046200020204     C                   IF        vlrppt > *blanks
046300040405     C                   IF        %subst(vlrppt:1:7) <> *blanks
046400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
046500020204     C                   EXSR      CHKNUM
046600020204     C                   IF        PiInt=*on
046700020204     C                   Z-ADD     PiVal         VABCCM
046800060626     C                   Z-ADD     PiVal         VATCCM
046900020204     C                   ENDIF
047000040405     C                   ENDIF
047100040405     C                   IF        %subst(vlrppt:8:3) <> *blanks
047200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
047300020204     C                   EXSR      CHKNUM
047400020204     C                   IF        PiInt=*on
047500020204     C                   Z-ADD     PiVal         VABLNP
047600060626     C                   Z-ADD     PiVal         VATLNP
047700020204     C                   ENDIF
047800040405     C                   ENDIF
047900040405     C                   IF        %subst(vlrppt:11:3) <> *blanks
048000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
048100020204     C                   EXSR      CHKNUM
048200020204     C                   IF        PiInt=*on
048300020204     C                   Z-ADD     PiVal         VABCTR
048400020204     C                   ENDIF
048500040405     C                   ENDIF
048600060626     C                   IF        %subst(vlrppt:14:1) = 'C'                    * Disk C
048700060626     C                   MOVEL     'C'           wDISK             1
048800060626     C                   EVAL      VABCTM = '7Q'
048900060626     C                   ENDIF
049000071009     C                   IF        %subst(vlrppt:15:1) <> *blanks               * Consegne ai piani
049100071009     C                   EVAL      VABTC1 = %subst(vlrppt:15:1)
049200071009     C                   ENDIF
049300101201     C                   MOVEL     *blanks       wNEW              1
049400101201     C                   IF        %subst(vlrppt:16:1) <> *blanks               * modo "nuovo"
049500101201     C                   EVAL      wNEW = %subst(vlrppt:16:1)
049600101201     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900010330     C                   ENDSR
050000010607     C*----------------------------------------------------*
050100030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
050200010607     C*----------------------------------------------------*
050300010607     C     IMPVAB        BEGSR
050400010607     C*
050500010607     C                   SETOFF                                       3132
050600030327     C*
050700030327     C                   MOVEL     datcor        VABAAS
050800030327     C                   MOVE      datcor        VABMGS
050900030327     C*
051000040405     C                   EVAL      VABRSD=%trim(%subst(vindta:8:24))
051100030327     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
051200030327     C     '@':'A'       XLATE     VABRSD        VABRSD
051300030327     C* ==
051400070314     C                   EVAL      VABIND=%trim(%subst(vindta:32:25))
051500040405     C                   EVAL      VABLOD=%trim(%subst(vindta:62:15))
051600040405     C                   EVAL      VABPRD=%trim(%subst(vindta:77:2))
051700040405     C                   EVAL      VABRMA=%trim(%subst(vindta:79:7))
051800040405     C                   EVAL      VABCBO=%trim(%subst(vindta:95:1))
051900070314     C                   EVAL      VABNT2 = %trim(%subst(vindta:162:5))+' '+
052000070314     C                                      %trim(%subst(vindta:167:10))
052100100714     C                   EVAL      VABTC2 = %subst(vindta:161:1)
052200030327     C* CAP
052300040405     C                   EVAL      PiStr=%trim(%subst(vindta:57:5))
052400030327     C                   EXSR      CHKNUM
052500030327     C                   IF        PiInt=*on
052600030327     C                   Z-ADD     PiVal         Num5_0
052700030327     C                   MOVEL     Num5_0        VABCAD
052800030327     C                   ELSE
052900030327     C                   SETON                                        32
053000030327     C                   EVAL      vinmsg = %trimr(vinmsg)
053100030327     C                             + ' ' + 'VABCAD'
053200030327     C                   ENDIF
053300030327     C* NSP/RMN
053400040405     C                   EVAL      PiStr=%trim(%subst(vindta:79:7))
053500030327     C                   EXSR      CHKNUM
053600030327     C                   IF        PiInt=*on
053700030327     C                   Z-ADD     PiVal         VABRMN
053800030327     C                   Z-ADD     PiVal         Num7_0
053900030327     C                   MOVE(P)   Num7_0        VABNSP
054000030327     C                   ELSE
054100030327     C                   SETON                                        31
054200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
054300030327     C                             + ' ' + 'VABNSP VABRMN'
054400030327     C                   ENDIF
054500030327     C* NCL
054600040405     C                   EVAL      PiStr=%trim(%subst(vindta:103:3))
054700030327     C                   EXSR      CHKNUM
054800030327     C                   IF        PiInt=*on
054900030327     C                   Z-ADD     PiVal         VABNCL
055000030327     C                   ELSE
055100030327     C                   SETON                                        32
055200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
055300030327     C                             + ' ' + 'VABNCL'
055400030327     C                   ENDIF
055500030327     C* PKB
055600040405     C                   EVAL      PiStr=%trim(%subst(vindta:106:7))
055700030327     C                   EXSR      CHKNUM
055800040405     C                   IF        PiNum=*on
055900040405     C                   EVAL(H)   VABPKB = PIVAL/1000                          * da gr. a kg.
056000030327     C                   ELSE
056100030327     C                   SETON                                        32
056200030327     C                   EVAL      vinmsg = %trimr(vinmsg)
056300030327     C                             + ' ' + 'VABPKB'
056400030327     C                   ENDIF
056500030327     C*
056600030327     C* Considerazioni sul contenuto di campi precedentemente valorizzati
056700030327     C                   IF        VABCAS > *zeros
056800040405     C                   IF        VABCBO = '1'
056900030327     C                   EVAL      VABCBO = '4'
057000040405     C                   ENDIF
057100040405     C                   IF        VABCBO = '2'
057200040405     C                   EVAL      VABCBO = '6'
057300040405     C                   ENDIF
057400030327     C                   ENDIF
057500060908     C*
057600060908     C* Considerazioni particolari in caso d porto assegnato
057700060908     C                   IF        VABCBO = '2' OR
057800060908     C                             VABCBO = '6'
057900060908     C                   EVAL      VABFTM = 'S'
058000060908     C                   ENDIF
058100030327     C*
058200030327     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058300030327     C                   EXSR      CHKIMPDIV
058400020204     C*
058500010607     C                   ENDSR
058600101129     C*----------------------------------------------------*
058700101129     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
058800101129     C*----------------------------------------------------*
058900101129     C     IMPVAB2       BEGSR
059000101129     C*
059100101129     C                   SETOFF                                       3132
059200101129     C*
059300101129     C                   MOVEL     datcor        VABAAS
059400101129     C                   MOVE      datcor        VABMGS
059500101129     C*
059600101201     C                   EVAL      VABRSD=%trim(%subst(vindta:11:35))
059700101129     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
059800101129     C     '@':'A'       XLATE     VABRSD        VABRSD
059900101129     C* ==
060000101129     C                   EVAL      VABIND=%trim(%subst(vindta:46:35))
060100101129     C                   EVAL      VABLOD=%trim(%subst(vindta:86:35))
060200101129     C                   EVAL      VABPRD=%trim(%subst(vindta:121:2))
060300101129     C                   EVAL      VABRMA=%trim(%subst(vindta:123:7))
060400101129     C                   EVAL      VABCBO=%trim(%subst(vindta:139:1))
060500101201     C                   EVAL      VABTC2 = %subst(vindta:205:1)
060600101129     C* CAP
060700101129     C                   EVAL      PiStr=%trim(%subst(vindta:81:5))
060800101129     C                   EXSR      CHKNUM
060900101129     C                   IF        PiInt=*on
061000101129     C                   Z-ADD     PiVal         Num5_0
061100101129     C                   MOVEL     Num5_0        VABCAD
061200101129     C                   ELSE
061300101129     C                   SETON                                        32
061400101129     C                   EVAL      vinmsg = %trimr(vinmsg)
061500101129     C                             + ' ' + 'VABCAD'
061600101129     C                   ENDIF
061700101129     C* NSP/RMN
061800101129     C                   EVAL      PiStr=%trim(%subst(vindta:123:7))
061900101129     C                   EXSR      CHKNUM
062000101129     C                   IF        PiInt=*on
062100101129     C                   Z-ADD     PiVal         VABRMN
062200101129     C                   Z-ADD     PiVal         Num7_0
062300101129     C                   MOVE(P)   Num7_0        VABNSP
062400101129     C                   ELSE
062500101129     C                   SETON                                        31
062600101129     C                   EVAL      vinmsg = %trimr(vinmsg)
062700101129     C                             + ' ' + 'VABNSP VABRMN'
062800101129     C                   ENDIF
062900101129     C* NCL
063000101129     C                   EVAL      PiStr=%trim(%subst(vindta:147:3))
063100101129     C                   EXSR      CHKNUM
063200101129     C                   IF        PiInt=*on
063300101129     C                   Z-ADD     PiVal         VABNCL
063400101129     C                   ELSE
063500101129     C                   SETON                                        32
063600101129     C                   EVAL      vinmsg = %trimr(vinmsg)
063700101129     C                             + ' ' + 'VABNCL'
063800101129     C                   ENDIF
063900101129     C* PKB
064000101129     C                   EVAL      PiStr=%trim(%subst(vindta:150:7))
064100101129     C                   EXSR      CHKNUM
064200101129     C                   IF        PiNum=*on
064300101129     C                   EVAL(H)   VABPKB = PIVAL/1000                          * da gr. a kg.
064400101129     C                   ELSE
064500101129     C                   SETON                                        32
064600101129     C                   EVAL      vinmsg = %trimr(vinmsg)
064700101129     C                             + ' ' + 'VABPKB'
064800101129     C                   ENDIF
064900101129     C*
065000101129     C* Considerazioni sul contenuto di campi precedentemente valorizzati
065100101129     C                   IF        VABCAS > *zeros
065200101129     C                   IF        VABCBO = '1'
065300101129     C                   EVAL      VABCBO = '4'
065400101129     C                   ENDIF
065500101129     C                   IF        VABCBO = '2'
065600101129     C                   EVAL      VABCBO = '6'
065700101129     C                   ENDIF
065800101129     C                   ENDIF
065900101129     C*
066000101129     C* Considerazioni particolari in caso d porto assegnato
066100101129     C                   IF        VABCBO = '2' OR
066200101129     C                             VABCBO = '6'
066300101129     C                   EVAL      VABFTM = 'S'
066400101129     C                   ENDIF
066500101129     C*
066600101129     C* Eseguo routine finale x considerazioni specifiche su importi/divise
066700101129     C                   EXSR      CHKIMPDIV
066800101129     C*
066900101129     C                   ENDSR
067000101129     C*----------------------------------------------------*
067100010601
067200020204
067300020204     C*----------------------------------------------------*
067400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
067500020204     C*----------------------------------------------------*
067600020204     C     CHKIMPDIV     BEGSR
067700020204     C*
067800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067900020204     C                   Z-ADD     *zeros        wrkDec            9 9
068000020204     C*
068100020204     C* Come prima cosa effettuo considerazioni sulla divisa
068200020204     C                   IF        vabIAS > *zeros
068300020204     C                   IF        vabVAS <> 'EUR'
068400020204     C                   EVAL      vabVAS =  'ITL'
068500020204     C                   ENDIF
068600020204     C                   ENDIF
068700020204     C*
068800020204     C                   IF        vabCAS > *zeros
068900020204     C                   IF        vabVCA <> 'EUR'
069000020204     C                   EVAL      vabVCA =  'ITL'
069100020204     C                   ENDIF
069200020204     C                   ENDIF
069300020204     C*
069400020204     C                   IF        vabVMD > *zeros
069500020321     C                   IF        vabVAD <> 'EUR'
069600020204     C                   EVAL      vabVAD =  'ITL'
069700020204     C                   ENDIF
069800020204     C                   ENDIF
069900020204     C*
070000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070100020204     C                   Z-ADD     vabIAS        wrkDec
070200020204     C                   IF        wrkDec > *zeros
070300020204     C                   IF        vabVAS = 'ITL'
070400020204     C                   EVAL      vabIAS = *zeros
070500020204     C                   ENDIF
070600020204     C                   ENDIF
070700020204     C*
070800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
070900020204     C                   Z-ADD     vabCAS        wrkDec
071000020204     C                   IF        wrkDec > *zeros
071100020204     C                   IF        vabVCA = 'ITL'
071200020204     C                   EVAL      vabCAS = *zeros
071300020204     C                   ENDIF
071400020204     C                   ENDIF
071500020204     C*
071600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071700020204     C                   Z-ADD     vabVMD        wrkDec
071800020204     C                   IF        wrkDec > *zeros
071900020204     C                   IF        vabVAD = 'ITL'
072000020204     C                   EVAL      vabVMD = *zeros
072100020204     C                   ENDIF
072200020204     C                   ENDIF
072300020204     C*
072400020204     C                   ENDSR
072500020204     C***
072600020204
072700010330
072800010330
072900010330     C*----------------------------------------------------*
073000010330     C*  CONTROLLO NUMERICITA' CAMPI
073100010330     C*----------------------------------------------------*
073200010330     C     CHKNUM        BEGSR
073300010330     C*
073400010606     C                   IF        PiDecChr = *blanks
073500030327     C                   EVAL      PiDecChr = ','
073600010606     C                   ENDIF
073700010606     C*
073800010606     C                   CALL(e)   'ISNUMERIC'
073900010330     C                   PARM                    PiStr            30
074000010606     C                   PARM                    PiDecChr          1
074100010330     C                   PARM      *ZEROS        PiVal            30 9
074200010330     C                   PARM      '0'           PiInt             1
074300010330     C                   PARM      '0'           PiNum             1
074400010330     C                   IF        %error
074500030327     C                   EVAL      PiInt=*off
074600010606     C                   EVAL      PiNum=*off
074700010330     C                   ENDIF
074800010330     C*
074900010330     C                   ENDSR
075000010330     C***
075100010330
075200010601
075300010601
075400010601
075500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
075600010601     C     repfil        BEGSR
075700010601     C*
075800010601     C                   if        invfil = *zeros and
075900010601     C                             depfil > *zeros and
076000010629     C                             (vinflg = *blanks or
076100010629     C                              vinflg = *zeros)
076200010601     C
076300010601     C                   eval      invfil = depfil
076400010601     C                   endif
076500010601     C*
076600010601     C                   if        depfil <> invfil and
076700010601     C                             invfil > *zeros
076800010601     C                   eval      flgMulti = '1'
076900010601     C                   if        vinflg = *blanks
077000010601     C                   add       1             cntNonEl
077100010601     C                   endif
077200010601     C                   endif
077300010601     C*
077400010601     C                   if        vinflg = '2'
077500010601     C                   eval      flgStato = '2'
077600010601     C                   endif
077700010601     C*
077800010601     C                   ENDSR
077900010601     C***
078000010601
078100060626
078200060626
078300060626      /TITLE Invio dei dati al punto operativo.
078400060626     C     invio         BEGSR
078500060626     C*
078600060626     C* 1° invio FIVAT
078700060626     C                   reset                   dscmz
078800060626     C                   move      vlrpoi        cmzdst
078900060626     C                   eval      cmzfld = 'FIVATWWR'
079000060626     C                   eval      cmzmbd = vlrhdl
079100060626     C                   eval      %subst(cmzmbd:1:1) = 'M'
079200060626     C***                if        prmfir = *blanks
079300060626     C                   eval      cmzfla = 'FIVAT00F'
079400060626     C                   eval      cmzmba = 'FIVAT00F'
079500060626     C***                else
079600060626     C***                eval      cmzfla = prmfir
079700060626     C***                eval      cmzmba = prmfir
079800060626     C***                endif
079900060626     C                   eval      cmznrr = *zeros
080000060626     C                   move      §ctrokvt      cmznrr
080100060626     C                   eval      cmzlba = vlrfl1
080200060626     C                   call(e)   'TIS711C'
080300060626     C                   parm                    dscmz
080400060626     C                   parm      *blanks       esito
080500060626     C                   if        %error
080600060626     C                             or cmzerr = '1'
080700060626     C                             or esito  = '1'
080800060626     C                   eval      wrkesito = '3'
080900060626     C                   else
081000060626     C*
081100060626     C* 2° invio FIVAB
081200060626     C                   reset                   dscmz
081300060626     C                   move      vlrpoi        cmzdst
081400060626     C                   eval      cmzfld = vlrfou
081500060626     C                   eval      cmzmbd = vlrhdl
081600060626     C                   eval      %subst(cmzmbd:1:1) = 'M'
081700060626     C***                if        prmfir = *blanks
081800060626     C                   eval      cmzfla = 'FIVAB00F'
081900060626     C                   eval      cmzmba = 'FIVAB00F'
082000060626     C***                else
082100060626     C***                eval      cmzfla = prmfir
082200060626     C***                eval      cmzmba = prmfir
082300060626     C***                endif
082400060626     C                   eval      cmznrr = *zeros
082500060626     C                   move      §ctrokvb      cmznrr
082600060626     C                   eval      cmzlba = vlrfl1
082700060626     C                   call(e)   'TIS711C'
082800060626     C                   parm                    dscmz
082900060626     C                   parm      *blanks       esito
083000060626     C                   if        %error
083100060626     C                             or cmzerr = '1'
083200060626     C                             or esito  = '1'
083300060626     C                   eval      wrkesito = '3'
083400060626     C                   endif
083500060626     C                   endif
083600060626     C*
083700060626     C                   ENDSR
083800060626     C***
083900010601
084000060626
084100010601
084200010601      /TITLE Invio dei dati al punto operativo.
084300010601     C     opeini        BEGSR
084400010601     C*
084500010601     C* Inizializzo flag e contatori operativi
084600010601     C                   movel     '0'           flgGiro           1
084700010601     C                   movel     '0'           flgMulti          1
084800010601     C                   movel     '1'           flgStato          1
084900010615     C                   movel     '0'           flgOk             1
085000010601     C                   z-add     *zeros        cntNonEl         10 0
085100010601     C                   z-add     *zeros        depfil            3 0
085200010601     C                   z-add     *zeros        invfil            3 0
085300010601     C*
085400010601     C                   ENDSR
085500010601     C***
085600010601
085700010601
085800010601
085900010330
086000010330
086100000613     C     *inzsr        BEGSR
086200990910     C*
086300990910     C     *entry        plist
086400990920     C                   parm                    tivlrds
086500990921     C                   parm      wrkesito      esito
086600000724     C                   parm                    prmlit
086700000710     C                   parm                    prmfir
086800010330     C*
086900010330     C* CALCOLA LA DATA CORRENTE
087000100714     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087100100714     C                   eval      datcor = %dec(%date() : *ISO)
087200000613     C*
087300000613     C                   ENDSR
087400000613     C***
087500990908
087600040405     Otitvb1p   E            riepilogo         2
087700990915     O                                              'Upload via Internet'
087800990915     O                                           +1 'Traduzione TIVIN00R -'
087900030130     O                                           +1 'FIVABWWR'
088000990915     O                                           +1 'Testate bolle'
088100990915     O          E            riepilogo   2
088200990915     O                       wrkdata
088300990915     O                       wrkora              +1
088400990915     O                       procname            +1
088500990915     O          E            riepilogo   2
088600990915     O                                              'Cliente..................:'
088700010605     O                       VABCCM        z     +1
088800990915     O          E            riepilogo   2
088900990920     O                                              'Riferimento Strategi.....:'
089000990920     O                       vlrhdl              +1
089100990915     O          E            riepilogo   2
089200990915     O                                              'Giusti...................:'
089300060626     O                       §CTROKVB      2   +  1
089400990915     O          E            riepilogo   2
089500990915     O                                              'Sbagliati e corretti.....:'
089600971022     O                       §CTRMO        2   +  1
089700990915     O          E            riepilogo   2
089800990915     O                                              'Sbagliati e scartati.....:'
089900971022     O                       §CTRNO        2   +  1
090000000613
090100040405     Otitvb1ps  E            testdett          2
090200000613     O                                              'Upload via Internet'
090300000613     O                                           +1 'Traduzione TIVIN00R -'
090400030130     O                                           +1 'FIVABWWR'
090500000613     O                                           +1 'Testate bolle'
090600000616     O          E            testdett    3
090700000613     O                                           +2 'N° rec'
090800000613     O                                           +3 'Anteprima contenuto'
090900000616     O          E            rigadett    2
091000000613     O                       rrnum               +2
091100000621     O                       recko               +3
091200000616     O          E            findett     2
091300000613     O                       wrkdata
091400000613     O                       wrkora              +1
091500000613     O                       procname            +1
091600000616     O          E            findett     2
091700000613     O                                              'Cliente..................:'
091800010605     O                       VABCCM        z     +1
091900000616     O          E            findett     2
092000000613     O                                              'Riferimento Strategi.....:'
092100000613     O                       vlrhdl              +1
092200000616     O          E            findett     2
092300000613     O                                              'Giusti...................:'
092400060626     O                       §CTROKVB      2   +  1
092500000616     O          E            findett     2
092600000613     O                                              'Sbagliati e corretti.....:'
092700000613     O                       §CTRMO        2   +  1
092800000616     O          E            findett     2
092900000613     O                                              'Sbagliati e scartati.....:'
093000000613     O                       §CTRNO        2   +  1
093100000613** CMD - COMANDI CL
093200040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
093300040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
093400040405DLTOVR FILE(TITVB1P TITVB1PS) LVL(*)
093500010606
093600000613
