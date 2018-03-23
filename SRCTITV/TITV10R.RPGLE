000100021025      /TITLE Upload via Internet: traduzione in FIVATWWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVATwwr  O    E             DISK    usropn
000700020329     Ftitv10p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020329     Ftitv10ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200030326     D savBol          s             15    INZ(*blanks)
003300030326     D savNsp          s              7    INZ(*blanks)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
004300010330
004400010330
004500010330
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300020321     C                   exsr      rwvat
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
009700020329     C                   if        not %open(titv10ps)
009800020329     C                   open      titv10ps
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
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700990915     C*
011800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900000613     C*
012000020329     C                   if        %open(titv10ps)
012100000616     C                   except    findett
012200020329     C                   close     titv10ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000000616     C*
013100010601     C                   ENDSR
013200010601     C***
013300010601
013400010601
013500010601
013600010601
013700000613
013800000613     C*--------------------------------------------------------
013900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014000000613     C*--------------------------------------------------------
014100000621     C     STPR          BEGSR
014200000613     C*
014300020329     C                   if        not %open(titv10p)
014400020329     C                   open      titv10p
014500990915     C                   endif
014600990915     C*
014700990915     C                   except    riepilogo
014800990915     C*
014900020329     C                   if        %open(titv10p)
015000020329     C                   close     titv10p
015100990914     C                   endif
015200000613     C*
015300000613     C                   ENDSR
015400000613     C***
015500000613
015600010330
015700010601
015800010601
015900010601
016000010330     C*--------------------------------------------------------
016100021025     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
016200010330     C*--------------------------------------------------------
016300020321     C     RWVAT         BEGSR
016400010330     C*
016500010330     C                   if        not %open(tivin00r)
016600010330     C                   open      tivin00r
016700010330     C                   endif
016800021025     C                   if        not %open(fivatwwr)
016900021025     C                   open      fivatwwr
017000010330     C                   endif
017100010330     C*
017200010604     C                   clear                   §CTROK
017300010604     C                   clear                   §CTRMO
017400010604     C                   clear                   §CTRNO
017500010330     C*
017600010330     C                   DO        *HIVAL
017700010330     C*
017800010330     C                   READ      tivin00r                               70
017900010618     C*
018000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018100010618     C                   if        vinflg = '1'
018200010618     C                   eval      flgOk = '1'
018300010618     C                   endif
018400010618     C*
018500010330     C                   if        vindta > *blanks
018600010330     C                   add       1             rrnum
018700010330     C*
018800010601     C                   if        *in70 = *off and
018900010330     C                             (vinflg = *blanks
019000010330     C                              or vinflg = '0'
019100010330     C                              or vinflg = '2')
019200010330     C*
019300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019400010711     C                   if        vinflg = *blanks or vinflg = '0'
019500010711     C                   clear                   vinmsg
019600010711     C                   endif
019700010601     C*
019800010330     C                   exsr      inzvar
019900010330     C                   exsr      defcam
020000020321     C                   exsr      impvat
020100030326     C*
020200030326     C* Effettuo considerazioni sull'identificazione della bolla e del suo numero spedizione
020300030326     C                   if        %subst(vindta:1:15) = savBol
020400030326     C                   move(p)   savNsp        vatNSP
020500030326     C                   else
020600030326     C                   eval      savBol = %subst(vindta:1:15)
020700030326     C                   eval      savNsp = %subst(vindta:8:6)+
020800030326     C                                      %subst(vindta:15:1)
020900030326     C                   endif
021000010601     C*
021100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021200020321     C                   eval      depfil = VATLNP
021300010601     C                   exsr      repfil
021400010601     C                   if        depfil = invfil
021500021025     C                   if        vlrpoi = 999
021600021025     C                   MOVE(P)   invfil        VATFGS
021700021025     C                   else
021800021025     C                   MOVE(P)   vlrpoi        VATFGS
021900021025     C                   endif
022000010601     C*
022100010601     C                   exsr      PREELA
022200010601     C*
022300010604     C* Ebbene...
022400010604     C*
022500010604     C  N31              ADD       1             §CTROK            7 0
022600010604     C   32              ADD       1             §CTRMO            7 0
022700010604     C   31              ADD       1             §CTRNO            7 0
022800020205     C*
022900020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
023000021025     C  N31              WRITE     FIVAT000
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
023600010604     C                   eval      recko = vindta
023700010604     C                   except    rigadett
023800010604     C                   eval      vinflg = '2'
023900010604     C                   endif
024000010604     C                   endif
024100010601     C*
024200010604     C                   endif
024300010604     C*
024400010330     C                   else
024500010330     C                   eval      vinflg = '1'
024600010330     C                   endif
024700010601     C*
024800010601     C  N70              update    tivin000
024900010330     C*
025000010330     C  N70              ENDdo
025100010601     C
025200010601     C*
025300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025400010601     C                   if        cntNonEl = *zeros or
025500010601     C                             flgMulti = '0'
025600010330     C* Se non ci sono record con errori ...
025700010601     C                   if        §ctrno = 0 and
025800010604     C                             §ctrmo = 0 and
025900010601     C                             flgStato <> '2'
026000010330     C* ... restituisco esito OK.
026100010330     C                   eval      wrkesito = '0'
026200010330     C                   else
026300010330     C                   if        §ctrok > 0
026400010330     C                   eval      wrkesito = '1'
026500010330     C                   else
026600010615     C                   if        flgOk = '0'
026700010615     C                   eval      wrkesito = '2'
026800010615     C                   else
026900010615     C                   eval      wrkesito = '6'
027000010615     C                   endif
027100010330     C                   endif
027200010330     C                   endif
027300010601     C                   else
027400010601     C                   eval      wrkesito = '9'
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        %open(tivin00r)
027800010330     C                   close     tivin00r
027900010330     C                   endif
028000021025     C                   if        %open(fivatwwr)
028100021025     C                   close     fivatwwr
028200010330     C                   endif
028300010601     C*
028400010601     C                   if        vlrpoi <> 999
028500010601     C                   eval      invfil = vlrpoi
028600010601     C                   endif
028700010330     C*
028800010330     C                   if        §ctrok > 0
028900010601     C                             and invfil > *zeros
029000010330     C                   exsr      invio
029100010330     C                   endif
029200010601     C*
029300010618     C                   if        flgGiro = '1'
029400010601     C                   exsr      endela
029500010618     C                   endif
029600010330     C*
029700010330     C                   ENDSR
029800010330     C***
029900010330
030000010601
030100010601
030200010601
030300010601
030400010330     C*----------------------------------------------------*
030500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600010330     C*----------------------------------------------------*
030700020321 xxx C     INZVAR        BEGSR
030800010330     C*
030900010330     C*
031000010330     C                   ENDSR
031100010330     C*----------------------------------------------------*
031200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031300010330     C*----------------------------------------------------*
031400010330     C     DEFCAM        BEGSR
031500010330     C*
031600020204     C* Inizializzo il buffer del record da scrivere
031700021025     C                   CLEAR                   FIVAT000
031800020204     C* Imposto i valori di default...
031900020329     C                   EVAL      VATCCM = 0591254
032000020321     C                   EVAL      VATLNP = 059
032100020321     C                   EVAL      VATTRC = 'E'
032200020204     C* ... e poi verifico se sono stati passati come parametri
032300020204     C                   IF        vlrppt > *blanks
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020321     C                   Z-ADD     PiVal         VATCCM
032800020204     C                   ENDIF
032900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033000020204     C                   EXSR      CHKNUM
033100020204     C                   IF        PiInt=*on
033200020321     C                   Z-ADD     PiVal         VATLNP
033300020204     C                   ENDIF
033400020321     C                   ENDIF
033500020204     C*
033600010330     C                   ENDSR
033700010607     C*----------------------------------------------------*
033800021025 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033900010607     C*----------------------------------------------------*
034000020321     C     IMPVAT        BEGSR
034100010607     C*
034200010607     C                   SETOFF                                       3132
034300010607     C*
034400010607     C* Reperimento campi ALFA
034500020404     C                   EVAL      VATNOT='1'+%trim(%subst(vindta:34:4))+
034600020404     C                                        %trim(%subst(vindta:40:6))+
034700020404     C                                        %trim(%subst(vindta:51:4))
034800020329     C*
034900020329     C                   MOVEL     DATCOR        VATAAS
035000010607     C*
035100010607     C* Reperimento campi NUMERICI
035200030326     C                   EVAL      PiStr=%trim(%subst(vindta:8:6)+
035300030326     C                                         %subst(vindta:15:1))
035400020304     C                   EXSR      CHKNUM
035500020304     C                   IF        PiInt=*on
035600020321     C                   Z-ADD     PiVal         VATNSP
035700020304     C                   ELSE
035800020304     C                   SETON                                        32
035900020321     C                   Z-ADD     *zeros        VATNSP
036000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036100020321     C                             + ' ' + 'VATNSP'
036200020304     C                   ENDIF
036300020204     C*
036400010607     C                   ENDSR
036500010607     C*----------------------------------------------------*
036600010601
036700010330
036800010330     C*----------------------------------------------------*
036900010330     C*  CONTROLLO NUMERICITA' CAMPI
037000010330     C*----------------------------------------------------*
037100010330     C     CHKNUM        BEGSR
037200010330     C*
037300010606     C                   IF        PiDecChr = *blanks
037400010606     C                   EVAL      PiDecChr = ','
037500010606     C                   ENDIF
037600010606     C*
037700010606     C                   CALL(e)   'ISNUMERIC'
037800010330     C                   PARM                    PiStr            30
037900010606     C                   PARM                    PiDecChr          1
038000010330     C                   PARM      *ZEROS        PiVal            30 9
038100010330     C                   PARM      '0'           PiInt             1
038200010330     C                   PARM      '0'           PiNum             1
038300010330     C                   IF        %error
038400010606     C                   EVAL      PiNum=*off
038500010330     C                   ENDIF
038600010330     C*
038700010330     C                   ENDSR
038800010330     C***
038900010330
039000010601
039100010601
039200010601
039300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
039400010601     C     repfil        BEGSR
039500010601     C*
039600010601     C                   if        invfil = *zeros and
039700010601     C                             depfil > *zeros and
039800010629     C                             (vinflg = *blanks or
039900010629     C                              vinflg = *zeros)
040000010601     C
040100010601     C                   eval      invfil = depfil
040200010601     C                   endif
040300010601     C*
040400010601     C                   if        depfil <> invfil and
040500010601     C                             invfil > *zeros
040600010601     C                   eval      flgMulti = '1'
040700010601     C                   if        vinflg = *blanks
040800010601     C                   add       1             cntNonEl
040900010601     C                   endif
041000010601     C                   endif
041100010601     C*
041200010601     C                   if        vinflg = '2'
041300010601     C                   eval      flgStato = '2'
041400010601     C                   endif
041500010601     C*
041600010601     C                   ENDSR
041700010601     C***
041800010601
041900010601
042000010601
042100010330
042200010330
042300010330
042400990920      /TITLE Invio dei dati al punto operativo.
042500000613     C     invio         BEGSR
042600990920     C*
042700990920     C                   reset                   dscmz
042800010601     C                   move      invfil        cmzdst
042900990920     C                   eval      cmzfld = vlrfou
043000990920     C                   eval      cmzmbd = vlrhdl
043100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043200000710     C                   if        prmfir = *blanks
043300021025     C                   eval      cmzfla = 'FIVAT00F'
043400021025     C                   eval      cmzmba = 'FIVAT00F'
043500000710     C                   else
043600000710     C                   eval      cmzfla = prmfir
043700000710     C                   eval      cmzmba = prmfir
043800000710     C                   endif
043900990920     C                   eval      cmznrr = *zeros
044000990920     C                   move      §ctrok        cmznrr
044100021018     C                   eval      cmzlba = vlrfl1
044200990920     C                   call(e)   'TIS711C'
044300990920     C                   parm                    dscmz
044400990921     C                   parm      *blanks       esito
044500990920     C                   if        %error
044600990920     C                             or cmzerr = '1'
044700990921     C                             or esito  = '1'
044800000710     C                   eval      wrkesito = '3'
044900990920     C                   endif
045000990920     C*
045100000613     C                   ENDSR
045200990910
045300010601
045400010601
045500010601
045600010601
045700010601      /TITLE Invio dei dati al punto operativo.
045800010601     C     opeini        BEGSR
045900010601     C*
046000010601     C* Inizializzo flag e contatori operativi
046100010601     C                   movel     '0'           flgGiro           1
046200010601     C                   movel     '0'           flgMulti          1
046300010601     C                   movel     '1'           flgStato          1
046400010615     C                   movel     '0'           flgOk             1
046500010601     C                   z-add     *zeros        cntNonEl         10 0
046600010601     C                   z-add     *zeros        depfil            3 0
046700010601     C                   z-add     *zeros        invfil            3 0
046800010601     C*
046900010601     C                   ENDSR
047000010601     C***
047100010601
047200010601
047300010601
047400010330
047500010330
047600000613     C     *inzsr        BEGSR
047700990910     C*
047800990910     C     *entry        plist
047900990920     C                   parm                    tivlrds
048000990921     C                   parm      wrkesito      esito
048100000724     C                   parm                    prmlit
048200000710     C                   parm                    prmfir
048300010330     C*
048400010330     C* CALCOLA LA DATA CORRENTE
048500010330     C                   time                    wn14             14 0
048600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048700010330     C                   z-add     wn8           g08dat
048800010330     C                   z-add     *zeros        g08inv
048900010330     C                   movel     '0'           g08err
049000010330     C                   call      'XSRDA8'
049100010330     C                   parm                    wlbda8
049200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049300000613     C*
049400000613     C                   ENDSR
049500000613     C***
049600990908
049700020329     Otitv10p   E            riepilogo         2
049800990915     O                                              'Upload via Internet'
049900990915     O                                           +1 'Traduzione TIVIN00R -'
050000021025     O                                           +1 'FIVATWWR'
050100990915     O                                           +1 'Testate bolle'
050200990915     O          E            riepilogo   2
050300990915     O                       wrkdata
050400990915     O                       wrkora              +1
050500990915     O                       procname            +1
050600990915     O          E            riepilogo   2
050700990915     O                                              'Cliente..................:'
050800020321     O                       VATCCM        z     +1
050900990915     O          E            riepilogo   2
051000990920     O                                              'Riferimento Strategi.....:'
051100990920     O                       vlrhdl              +1
051200990915     O          E            riepilogo   2
051300990915     O                                              'Giusti...................:'
051400971022     O                       §CTROK        2   +  1
051500990915     O          E            riepilogo   2
051600990915     O                                              'Sbagliati e corretti.....:'
051700971022     O                       §CTRMO        2   +  1
051800990915     O          E            riepilogo   2
051900990915     O                                              'Sbagliati e scartati.....:'
052000971022     O                       §CTRNO        2   +  1
052100000613
052200020329     Otitv10ps  E            testdett          2
052300000613     O                                              'Upload via Internet'
052400000613     O                                           +1 'Traduzione TIVIN00R -'
052500021025     O                                           +1 'FIVATWWR'
052600000613     O                                           +1 'Testate bolle'
052700000616     O          E            testdett    3
052800000613     O                                           +2 'N° rec'
052900000613     O                                           +3 'Anteprima contenuto'
053000000616     O          E            rigadett    2
053100000613     O                       rrnum               +2
053200000621     O                       recko               +3
053300000616     O          E            findett     2
053400000613     O                       wrkdata
053500000613     O                       wrkora              +1
053600000613     O                       procname            +1
053700000616     O          E            findett     2
053800000613     O                                              'Cliente..................:'
053900020321     O                       VATCCM        z     +1
054000000616     O          E            findett     2
054100000613     O                                              'Riferimento Strategi.....:'
054200000613     O                       vlrhdl              +1
054300000616     O          E            findett     2
054400000613     O                                              'Giusti...................:'
054500000613     O                       §CTROK        2   +  1
054600000616     O          E            findett     2
054700000613     O                                              'Sbagliati e corretti.....:'
054800000613     O                       §CTRMO        2   +  1
054900000616     O          E            findett     2
055000000613     O                                              'Sbagliati e scartati.....:'
055100000613     O                       §CTRNO        2   +  1
055200000613** CMD - COMANDI CL
055300020329OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV10P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
055400020329OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV10PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
055500020329DLTOVR FILE(TITV10P TITV10PS) LVL(*)
055600010606
055700000613
