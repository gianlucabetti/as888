000100031121      /TITLE Upload via Internet: traduzione in EDIVATWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600031121     FEDIVATwr  O    E             DISK    usropn
000700031202     Ftitv80p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900031202     Ftitv80ps  O    f  198        PRINTER usropn
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
003400031121     D wNomeFile       s             30    INZ(*blanks)
003500010330     D*------------------
003600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700010330     D*------------------
003800010330     D WLBDA8          DS                  INZ
003900010330     D  G08DAT                 1      8  0
004000010330     D  G08INV                 9     16  0
004100010330     D  G08ERR                17     17
004200010330     D  G08TGI                18     22  0
004300000313     D*
004400010330
004500010330
004600010330
004700990915     C                   time                    wrkdata
004800990915     C                   time                    wrkora
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400020321     C                   exsr      rwvat
005500010601     C*
005600010601     C                   seton                                        lr
005700010601
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006300010601     C*--------------------------------------------------------
006400010601     C     PREELA        BEGSR
006500010601     C*
006600010601     C* SE OCCORRE SPEDIRE IN FILIALE
006700010601     C                   if        invfil <> *zeros and
006800010601     C                             flgGiro = '0'
006900010601     C*
007000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007100010601     C                   eval      flgGiro = '1'
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500010601     C     invfil        chain     azorg01l
007600000724     C                   if        %found
007700000724     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000724     C*
008000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000724     C                   PARM                    depcmd
008400000724     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000616     C*
008800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000616     C                   movel(p)  CMD(2)        depcmd
009000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000613     C*
009800031202     C                   if        not %open(titv80ps)
009900031202     C                   open      titv80ps
010000000616     C                   except    testdett
010100000613     C                   endif
010200010601     C*
010300010601     C                   endif
010400010601     C*
010500010601     C                   ENDSR
010600010601     C***
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100031202     C                   if        %open(titv80ps)
012200000616     C                   except    findett
012300031202     C                   close     titv80ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601
013800000613
013900000613     C*--------------------------------------------------------
014000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014100000613     C*--------------------------------------------------------
014200000621     C     STPR          BEGSR
014300000613     C*
014400031202     C                   if        not %open(titv80p)
014500031202     C                   open      titv80p
014600990915     C                   endif
014700990915     C*
014800990915     C                   except    riepilogo
014900990915     C*
015000031202     C                   if        %open(titv80p)
015100031202     C                   close     titv80p
015200990914     C                   endif
015300000613     C*
015400000613     C                   ENDSR
015500000613     C***
015600000613
015700010330
015800010601
015900010601
016000010601
016100010330     C*--------------------------------------------------------
016200031121     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
016300010330     C*--------------------------------------------------------
016400020321     C     RWVAT         BEGSR
016500010330     C*
016600010330     C                   if        not %open(tivin00r)
016700010330     C                   open      tivin00r
016800010330     C                   endif
016900031121     C                   if        not %open(edivatwr)
017000031121     C                   open      edivatwr
017100010330     C                   endif
017200010330     C*
017300010604     C                   clear                   §CTROK
017400010604     C                   clear                   §CTRMO
017500010604     C                   clear                   §CTRNO
017600010330     C*
017700010330     C                   DO        *HIVAL
017800010330     C*
017900010330     C                   READ      tivin00r                               70
018000010618     C*
018100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018200010618     C                   if        vinflg = '1'
018300010618     C                   eval      flgOk = '1'
018400010618     C                   endif
018500010618     C*
018600010330     C                   if        vindta > *blanks
018700010330     C                   add       1             rrnum
018800010330     C*
018900010601     C                   if        *in70 = *off and
019000010330     C                             (vinflg = *blanks
019100010330     C                              or vinflg = '0'
019200010330     C                              or vinflg = '2')
019300010330     C*
019400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019500010711     C                   if        vinflg = *blanks or vinflg = '0'
019600010711     C                   clear                   vinmsg
019700010711     C                   endif
019800010601     C*
019900010330     C                   exsr      inzvar
020000010330     C                   exsr      defcam
020100020321     C                   exsr      impvat
020200030326     C*
020300030326     C* Effettuo considerazioni sull'identificazione della bolla e del suo numero spedizione
020400030326     C                   if        %subst(vindta:1:15) = savBol
020500030326     C                   move(p)   savNsp        vatNSP
020600030326     C                   else
020700030326     C                   eval      savBol = %subst(vindta:1:15)
020800030326     C                   eval      savNsp = %subst(vindta:8:6)+
020900030326     C                                      %subst(vindta:15:1)
021000030326     C                   endif
021100010601     C*
021200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021300020321     C                   eval      depfil = VATLNP
021400010601     C                   exsr      repfil
021500010601     C                   if        depfil = invfil
021600021025     C                   if        vlrpoi = 999
021700021025     C                   MOVE(P)   invfil        VATFGS
021800021025     C                   else
021900021025     C                   MOVE(P)   vlrpoi        VATFGS
022000021025     C                   endif
022100010601     C*
022200010601     C                   exsr      PREELA
022300010601     C*
022400010604     C* Ebbene...
022500010604     C*
022600010604     C  N31              ADD       1             §CTROK            7 0
022700010604     C   32              ADD       1             §CTRMO            7 0
022800010604     C   31              ADD       1             §CTRNO            7 0
022900020205     C*
023000020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
023100031121     C  N31              WRITE     EDIVAT00
023200010604     C*
023300010604     C                   if        *in31 = *off and
023400010604     C                             *in32 = *off
023500010604     C                   eval      vinflg = '1'
023600010604     C                   else
023700010604     C                   eval      recko = vindta
023800010604     C                   except    rigadett
023900010604     C                   eval      vinflg = '2'
024000010604     C                   endif
024100010604     C                   endif
024200010601     C*
024300010604     C                   endif
024400010604     C*
024500010330     C                   else
024600010330     C                   eval      vinflg = '1'
024700010330     C                   endif
024800010601     C*
024900010601     C  N70              update    tivin000
025000010330     C*
025100010330     C  N70              ENDdo
025200010601     C
025300010601     C*
025400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025500010601     C                   if        cntNonEl = *zeros or
025600010601     C                             flgMulti = '0'
025700010330     C* Se non ci sono record con errori ...
025800010601     C                   if        §ctrno = 0 and
025900010604     C                             §ctrmo = 0 and
026000010601     C                             flgStato <> '2'
026100010330     C* ... restituisco esito OK.
026200010330     C                   eval      wrkesito = '0'
026300010330     C                   else
026400010330     C                   if        §ctrok > 0
026500010330     C                   eval      wrkesito = '1'
026600010330     C                   else
026700010615     C                   if        flgOk = '0'
026800010615     C                   eval      wrkesito = '2'
026900010615     C                   else
027000010615     C                   eval      wrkesito = '6'
027100010615     C                   endif
027200010330     C                   endif
027300010330     C                   endif
027400010601     C                   else
027500010601     C                   eval      wrkesito = '9'
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        %open(tivin00r)
027900010330     C                   close     tivin00r
028000010330     C                   endif
028100031121     C                   if        %open(edivatwr)
028200031121     C                   close     edivatwr
028300010330     C                   endif
028400010601     C*
028500010601     C                   if        vlrpoi <> 999
028600010601     C                   eval      invfil = vlrpoi
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        §ctrok > 0
029000010601     C                             and invfil > *zeros
029100010330     C                   exsr      invio
029200010330     C                   endif
029300010601     C*
029400010618     C                   if        flgGiro = '1'
029500010601     C                   exsr      endela
029600010618     C                   endif
029700010330     C*
029800010330     C                   ENDSR
029900010330     C***
030000010330
030100010601
030200010601
030300010601
030400010601
030500010330     C*----------------------------------------------------*
030600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700010330     C*----------------------------------------------------*
030800020321 xxx C     INZVAR        BEGSR
030900010330     C*
031000010330     C*
031100010330     C                   ENDSR
031200010330     C*----------------------------------------------------*
031300010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031400010330     C*----------------------------------------------------*
031500010330     C     DEFCAM        BEGSR
031600010330     C*
031700020204     C* Inizializzo il buffer del record da scrivere
031800031121     C                   CLEAR                   EDIVAT00
031900020204     C* Imposto i valori di default...
032000020329     C                   EVAL      VATCCM = 0591254
032100020321     C                   EVAL      VATLNP = 059
032200020321     C                   EVAL      VATTRC = 'E'
032300020204     C* ... e poi verifico se sono stati passati come parametri
032400020204     C                   IF        vlrppt > *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020321     C                   Z-ADD     PiVal         VATCCM
032900020204     C                   ENDIF
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020321     C                   Z-ADD     PiVal         VATLNP
033400020204     C                   ENDIF
033500020321     C                   ENDIF
033600020204     C*
033700010330     C                   ENDSR
033800010607     C*----------------------------------------------------*
033900031121 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
034000010607     C*----------------------------------------------------*
034100020321     C     IMPVAT        BEGSR
034200010607     C*
034300010607     C                   SETOFF                                       3132
034400010607     C*
034500010607     C* Reperimento campi ALFA
034600020404     C                   EVAL      VATNOT='1'+%trim(%subst(vindta:34:4))+
034700060201     C                                        %trim(%subst(vindta:39:7))+
034800020404     C                                        %trim(%subst(vindta:51:4))
034900020329     C*
035000020329     C                   MOVEL     DATCOR        VATAAS
035100010607     C*
035200010607     C* Reperimento campi NUMERICI
035300030326     C                   EVAL      PiStr=%trim(%subst(vindta:8:6)+
035400030326     C                                         %subst(vindta:15:1))
035500020304     C                   EXSR      CHKNUM
035600020304     C                   IF        PiInt=*on
035700020321     C                   Z-ADD     PiVal         VATNSP
035800020304     C                   ELSE
035900020304     C                   SETON                                        32
036000020321     C                   Z-ADD     *zeros        VATNSP
036100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036200020321     C                             + ' ' + 'VATNSP'
036300020304     C                   ENDIF
036400031121     C*
036500031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
036600031121     C                   MOVE(P)   vlrMSG        wNomeFile
036700031121     C                   EVAL      VATCMR = %subst(wNomeFile:2:7)
036800031121     C                   EVAL      VATCNT = 1
036900020204     C*
037000010607     C                   ENDSR
037100010607     C*----------------------------------------------------*
037200010601
037300010330
037400010330     C*----------------------------------------------------*
037500010330     C*  CONTROLLO NUMERICITA' CAMPI
037600010330     C*----------------------------------------------------*
037700010330     C     CHKNUM        BEGSR
037800010330     C*
037900010606     C                   IF        PiDecChr = *blanks
038000010606     C                   EVAL      PiDecChr = ','
038100010606     C                   ENDIF
038200010606     C*
038300010606     C                   CALL(e)   'ISNUMERIC'
038400010330     C                   PARM                    PiStr            30
038500010606     C                   PARM                    PiDecChr          1
038600010330     C                   PARM      *ZEROS        PiVal            30 9
038700010330     C                   PARM      '0'           PiInt             1
038800010330     C                   PARM      '0'           PiNum             1
038900010330     C                   IF        %error
039000010606     C                   EVAL      PiNum=*off
039100010330     C                   ENDIF
039200010330     C*
039300010330     C                   ENDSR
039400010330     C***
039500010330
039600010601
039700010601
039800010601
039900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040000010601     C     repfil        BEGSR
040100010601     C*
040200010601     C                   if        invfil = *zeros and
040300010601     C                             depfil > *zeros and
040400010629     C                             (vinflg = *blanks or
040500010629     C                              vinflg = *zeros)
040600010601     C
040700010601     C                   eval      invfil = depfil
040800010601     C                   endif
040900010601     C*
041000010601     C                   if        depfil <> invfil and
041100010601     C                             invfil > *zeros
041200010601     C                   eval      flgMulti = '1'
041300010601     C                   if        vinflg = *blanks
041400010601     C                   add       1             cntNonEl
041500010601     C                   endif
041600010601     C                   endif
041700010601     C*
041800010601     C                   if        vinflg = '2'
041900010601     C                   eval      flgStato = '2'
042000010601     C                   endif
042100010601     C*
042200010601     C                   ENDSR
042300010601     C***
042400010601
042500010601
042600010601
042700010330
042800010330
042900010330
043000990920      /TITLE Invio dei dati al punto operativo.
043100000613     C     invio         BEGSR
043200990920     C*
043300990920     C                   reset                   dscmz
043400010601     C                   move      invfil        cmzdst
043500990920     C                   eval      cmzfld = vlrfou
043600990920     C                   eval      cmzmbd = vlrhdl
043700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043800000710     C                   if        prmfir = *blanks
043900031121     C                   eval      cmzfla = 'EDIVAT0F'
044000031121     C                   eval      cmzmba = 'EDIVAT0F'
044100000710     C                   else
044200000710     C                   eval      cmzfla = prmfir
044300000710     C                   eval      cmzmba = prmfir
044400000710     C                   endif
044500990920     C                   eval      cmznrr = *zeros
044600990920     C                   move      §ctrok        cmznrr
044700021018     C                   eval      cmzlba = vlrfl1
044800990920     C                   call(e)   'TIS711C'
044900990920     C                   parm                    dscmz
045000990921     C                   parm      *blanks       esito
045100990920     C                   if        %error
045200990920     C                             or cmzerr = '1'
045300990921     C                             or esito  = '1'
045400000710     C                   eval      wrkesito = '3'
045500990920     C                   endif
045600990920     C*
045700000613     C                   ENDSR
045800990910
045900010601
046000010601
046100010601
046200010601
046300010601      /TITLE Invio dei dati al punto operativo.
046400010601     C     opeini        BEGSR
046500010601     C*
046600010601     C* Inizializzo flag e contatori operativi
046700010601     C                   movel     '0'           flgGiro           1
046800010601     C                   movel     '0'           flgMulti          1
046900010601     C                   movel     '1'           flgStato          1
047000010615     C                   movel     '0'           flgOk             1
047100010601     C                   z-add     *zeros        cntNonEl         10 0
047200010601     C                   z-add     *zeros        depfil            3 0
047300010601     C                   z-add     *zeros        invfil            3 0
047400010601     C*
047500010601     C                   ENDSR
047600010601     C***
047700010601
047800010601
047900010601
048000010330
048100010330
048200000613     C     *inzsr        BEGSR
048300990910     C*
048400990910     C     *entry        plist
048500990920     C                   parm                    tivlrds
048600990921     C                   parm      wrkesito      esito
048700000724     C                   parm                    prmlit
048800000710     C                   parm                    prmfir
048900010330     C*
049000010330     C* CALCOLA LA DATA CORRENTE
049100010330     C                   time                    wn14             14 0
049200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049300010330     C                   z-add     wn8           g08dat
049400010330     C                   z-add     *zeros        g08inv
049500010330     C                   movel     '0'           g08err
049600010330     C                   call      'XSRDA8'
049700010330     C                   parm                    wlbda8
049800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049900000613     C*
050000000613     C                   ENDSR
050100000613     C***
050200990908
050300031202     Otitv80p   E            riepilogo         2
050400990915     O                                              'Upload via Internet'
050500990915     O                                           +1 'Traduzione TIVIN00R -'
050600031121     O                                           +1 'EDIVATWR'
050700990915     O                                           +1 'Testate bolle'
050800990915     O          E            riepilogo   2
050900990915     O                       wrkdata
051000990915     O                       wrkora              +1
051100990915     O                       procname            +1
051200990915     O          E            riepilogo   2
051300990915     O                                              'Cliente..................:'
051400020321     O                       VATCCM        z     +1
051500990915     O          E            riepilogo   2
051600990920     O                                              'Riferimento Strategi.....:'
051700990920     O                       vlrhdl              +1
051800990915     O          E            riepilogo   2
051900990915     O                                              'Giusti...................:'
052000971022     O                       §CTROK        2   +  1
052100990915     O          E            riepilogo   2
052200990915     O                                              'Sbagliati e corretti.....:'
052300971022     O                       §CTRMO        2   +  1
052400990915     O          E            riepilogo   2
052500990915     O                                              'Sbagliati e scartati.....:'
052600971022     O                       §CTRNO        2   +  1
052700000613
052800031202     Otitv80ps  E            testdett          2
052900000613     O                                              'Upload via Internet'
053000000613     O                                           +1 'Traduzione TIVIN00R -'
053100031121     O                                           +1 'EDIVATWR'
053200000613     O                                           +1 'Testate bolle'
053300000616     O          E            testdett    3
053400000613     O                                           +2 'N° rec'
053500000613     O                                           +3 'Anteprima contenuto'
053600000616     O          E            rigadett    2
053700000613     O                       rrnum               +2
053800000621     O                       recko               +3
053900000616     O          E            findett     2
054000000613     O                       wrkdata
054100000613     O                       wrkora              +1
054200000613     O                       procname            +1
054300000616     O          E            findett     2
054400000613     O                                              'Cliente..................:'
054500020321     O                       VATCCM        z     +1
054600000616     O          E            findett     2
054700000613     O                                              'Riferimento Strategi.....:'
054800000613     O                       vlrhdl              +1
054900000616     O          E            findett     2
055000000613     O                                              'Giusti...................:'
055100000613     O                       §CTROK        2   +  1
055200000616     O          E            findett     2
055300000613     O                                              'Sbagliati e corretti.....:'
055400000613     O                       §CTRMO        2   +  1
055500000616     O          E            findett     2
055600000613     O                                              'Sbagliati e scartati.....:'
055700000613     O                       §CTRNO        2   +  1
055800000613** CMD - COMANDI CL
055900031202OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV80P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
056000031202OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV80PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
056100031202DLTOVR FILE(TITV80P TITV80PS) LVL(*)
056200010606
056300000613
