000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR. (x EMMEGI - Cod. 0010051)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010910     Ftis7lyp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010910     Ftis7lyps  O    f  198        PRINTER usropn
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
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010910     D*------------------
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500010910     C                   if        not %open(tis7lyps)
009600010910     C                   open      tis7lyps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010910     C                   if        %open(tis7lyps)
011900000616     C                   except    findett
012000010910     C                   close     tis7lyps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100010910     C                   if        not %open(tis7lyp)
014200010910     C                   open      tis7lyp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700010910     C                   if        %open(tis7lyp)
014800010910     C                   close     tis7lyp
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021113     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021113     C                   if        not %open(fivabwwr)
016700021113     C                   open      fivabwwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021113     C                   if        vlrpoi = 999
020500021113     C                   MOVE(P)   invfil        VABFGS
020600021113     C                   else
020700021113     C                   MOVE(P)   vlrpoi        VABFGS
020800021113     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021113     C  N31              WRITE     FIVAB000
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021113     C                   if        %open(fivabwwr)
026800021113     C                   close     fivabwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010618     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600010330     C*
029700010330     C                   ENDSR
029800010330     C*----------------------------------------------------*
029900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030000010330     C*----------------------------------------------------*
030100010330     C     DEFCAM        BEGSR
030200010330     C*
030300021113     C                   CLEAR                   FIVAB000
030400020201     C                   MOVEL     *blanks       DEPCCM            7
030500020201     C                   EVAL      DEPCCM = %trim(VLRPPT)
030600020201     C                   MOVE(p)   DEPCCM        VABCCM
030700010910     C                   EVAL      VABLNP = 040
030800010605     C                   EVAL      VABCTR = 000
030900060504     C                   EVAL      VABCTM = '7Q'
031000010330     C*
031100010330     C                   ENDSR
031200010330     C*----------------------------------------------------*
031300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031400010330     C*----------------------------------------------------*
031500010605     C     IMPVAB        BEGSR
031600010330     C*
031700010604     C                   SETOFF                                       3132
031800010605     C                   MOVEL     datcor        VABAAS
031900010607     C                   MOVE      datcor        VABMGS
032000010330     C*
032100010330     C* Reperimento campi ALFA
032200010910     C                   EVAL      VABCBO  ='1 '
032300071102     C                   EVAL      VABRSD='ALLIANZ/LLOYD ADRIATICO AG.NR.'+
032400010910     C                                    %trim(%subst(vindta:2:10))
032500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032700020117     C* ==
032800010910     C                   EVAL      VABRMA=%trim(%subst(vindta:2:10))
032900010910     C                   IF        %subst(vindta:196:5) <> *blanks
033000010910     C                   EVAL      VABRD2='TEL.N.'                    +
033100010910     C                                    %trim(%subst(vindta:196:5)) +
033200010910     C                                    %trim(%subst(vindta:201:10))
033300010910     C                   ENDIF
033400010910     C                   EVAL      VABIND=%trim(%subst(vindta:42:50))
033500010910     C                   EVAL      VABLOD=%trim(%subst(vindta:97:30))
033600010910     C                   EVAL      VABCAD=%trim(%subst(vindta:92:5))
033700010910     C                   EVAL      VABPRD=%trim(%subst(vindta:127:2))
033800010910     C                   IF        %subst(vindta:129:37) <> *blanks
033900010910     C                   EVAL      VABTC1='P'
034000010910     C                   EVAL      VABNOT=%trim(%subst(vindta:129:37))
034100010910     C                   ENDIF
034200060504     C                   IF        %subst(vindta:228:1) = 'E'
034300060504     C                   EVAL      VABTSP = 'E'
034400060504     C                   ENDIF
034500060504     C                   EVAL      VABNAS=%trim(%subst(vindta:242:35))
034600010330     C*
034700010910     C                   EVAL      PiStr=%trim(%subst(vindta:166:10))
034800010330     C                   EXSR      CHKNUM
034900010330     C                   IF        PiInt=*on
035000010605     C                   Z-ADD     PiVal         VABNCL
035100010330     C                   ELSE
035200010605     C                   SETON                                        32
035300010605     C                   Z-ADD     *zeros        VABNCL
035400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
035500010605     C                             + ' ' + 'VABNCL'
035600010330     C                   ENDIF
035700010605     C*
035800010910     C                   EVAL      PiStr=%trim(%subst(vindta:176:10))
035900010605     C                   EXSR      CHKNUM
036000010605     C                   IF        PiNum=*on
036100010606     C                   Z-ADD(H)  PiVal         VABPKB
036200010605     C                   ELSE
036300010605     C                   SETON                                        32
036400010605     C                   Z-ADD     *zeros        VABPKB
036500010605     C                   EVAL      vinmsg = %trimr(vinmsg)
036600010605     C                             + ' ' + 'VABPKB'
036700010605     C                   ENDIF
036800060504     C*
036900060504     C                   EVAL      PiStr=%trim(%subst(vindta:229:13))
037000060504     C                   EXSR      CHKNUM
037100060504     C                   IF        PiNum=*on
037200060504     C                   Z-ADD(H)  PiVal         VABIAS
037300060504     C                   EVAL      VABVAS = 'EUR'
037400060504     C                   ELSE
037500060504     C                   SETON                                        32
037600060504     C                   Z-ADD     *zeros        VABIAS
037700060504     C                   EVAL      vinmsg = %trimr(vinmsg)
037800060504     C                             + ' ' + 'VABIAS'
037900060504     C                   ENDIF
038000010605     C*
038100010910     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
038200010605     C                   EXSR      CHKNUM
038300010605     C                   IF        PiInt=*on
038400010606     C                   Z-ADD     PiVal         VABRMN
038500010605     C                   ELSE
038600010605     C                   SETON                                        32
038700010605     C                   Z-ADD     *zeros        VABRMN
038800010605     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010720     C                             + ' ' + 'VABRMN'
039000010605     C                   ENDIF
039100060505     C*
039200060505     C                   EVAL      PiStr=%trim(%subst(vindta:221:7))
039300060505     C                   EXSR      CHKNUM
039400060505     C                   IF        PiInt=*on
039500060505     C                   Z-ADD     PiVal         VABNSP
039600060505     C                   ELSE
039700060505     C                   SETON                                        31
039800060505     C                   Z-ADD     *zeros        VABNSP
039900060505     C                   EVAL      vinmsg = %trimr(vinmsg)
040000060505     C                             + ' ' + 'VABNSP'
040100060505     C                   ENDIF
040200010330     C*
040300010330     C                   ENDSR
040400990920
040500010601
040600010601
040700010330
040800010330
040900010330     C*----------------------------------------------------*
041000010330     C*  CONTROLLO NUMERICITA' CAMPI
041100010330     C*----------------------------------------------------*
041200010330     C     CHKNUM        BEGSR
041300010330     C*
041400010606     C                   IF        PiDecChr = *blanks
041500010606     C                   EVAL      PiDecChr = ','
041600010606     C                   ENDIF
041700010606     C*
041800010606     C                   CALL(e)   'ISNUMERIC'
041900010330     C                   PARM                    PiStr            30
042000010606     C                   PARM                    PiDecChr          1
042100010330     C                   PARM      *ZEROS        PiVal            30 9
042200010330     C                   PARM      '0'           PiInt             1
042300010330     C                   PARM      '0'           PiNum             1
042400010330     C                   IF        %error
042500010606     C                   EVAL      PiNum=*off
042600010330     C                   ENDIF
042700010330     C*
042800010330     C                   ENDSR
042900010330     C***
043000010330
043100010601
043200010601
043300010601
043400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043500010601     C     repfil        BEGSR
043600010601     C*
043700010601     C                   if        invfil = *zeros and
043800010601     C                             depfil > *zeros and
043900010628     C                             (vinflg = *blanks or
044000010628     C                              vinflg = *zeros)
044100010601     C
044200010601     C                   eval      invfil = depfil
044300010601     C                   endif
044400010601     C*
044500010601     C                   if        depfil <> invfil and
044600010601     C                             invfil > *zeros
044700010601     C                   eval      flgMulti = '1'
044800010601     C                   if        vinflg = *blanks
044900010601     C                   add       1             cntNonEl
045000010601     C                   endif
045100010601     C                   endif
045200010601     C*
045300010601     C                   if        vinflg = '2'
045400010601     C                   eval      flgStato = '2'
045500010601     C                   endif
045600010601     C*
045700010601     C                   ENDSR
045800010601     C***
045900010601
046000010601
046100010601
046200010330
046300010330
046400010330
046500990920      /TITLE Invio dei dati al punto operativo.
046600000613     C     invio         BEGSR
046700990920     C*
046800990920     C                   reset                   dscmz
046900010601     C                   move      invfil        cmzdst
047000990920     C                   eval      cmzfld = vlrfou
047100990920     C                   eval      cmzmbd = vlrhdl
047200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047300000710     C                   if        prmfir = *blanks
047400021113     C                   eval      cmzfla = 'FIVAB00F'
047500021113     C                   eval      cmzmba = 'FIVAB00F'
047600000710     C                   else
047700000710     C                   eval      cmzfla = prmfir
047800000710     C                   eval      cmzmba = prmfir
047900000710     C                   endif
048000990920     C                   eval      cmznrr = *zeros
048100990920     C                   move      §ctrok        cmznrr
048200021025     C                   eval      cmzlba = vlrfl1
048300990920     C                   call(e)   'TIS711C'
048400990920     C                   parm                    dscmz
048500990921     C                   parm      *blanks       esito
048600990920     C                   if        %error
048700990920     C                             or cmzerr = '1'
048800990921     C                             or esito  = '1'
048900000710     C                   eval      wrkesito = '3'
049000990920     C                   endif
049100990920     C*
049200000613     C                   ENDSR
049300990910
049400010601
049500010601
049600010601
049700010601
049800010601      /TITLE Invio dei dati al punto operativo.
049900010601     C     opeini        BEGSR
050000010601     C*
050100010601     C* Inizializzo flag e contatori operativi
050200010601     C                   movel     '0'           flgGiro           1
050300010601     C                   movel     '0'           flgMulti          1
050400010601     C                   movel     '1'           flgStato          1
050500010615     C                   movel     '0'           flgOk             1
050600010601     C                   z-add     *zeros        cntNonEl         10 0
050700010601     C                   z-add     *zeros        depfil            3 0
050800010601     C                   z-add     *zeros        invfil            3 0
050900010601     C*
051000010601     C                   ENDSR
051100010601     C***
051200010601
051300010601
051400010601
051500010330
051600010330
051700000613     C     *inzsr        BEGSR
051800990910     C*
051900990910     C     *entry        plist
052000990920     C                   parm                    tivlrds
052100990921     C                   parm      wrkesito      esito
052200000724     C                   parm                    prmlit
052300000710     C                   parm                    prmfir
052400010330     C*
052500010330     C* CALCOLA LA DATA CORRENTE
052600010330     C                   time                    wn14             14 0
052700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052800010330     C                   z-add     wn8           g08dat
052900010330     C                   z-add     *zeros        g08inv
053000010330     C                   movel     '0'           g08err
053100010330     C                   call      'XSRDA8'
053200010330     C                   parm                    wlbda8
053300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
053400000613     C*
053500000613     C                   ENDSR
053600000613     C***
053700990908
053800010910     Otis7lyp   E            riepilogo         2
053900990915     O                                              'Upload via Internet'
054000990915     O                                           +1 'Traduzione TIVIN00R -'
054100021113     O                                           +1 'FIVABWWR'
054200990915     O                                           +1 'Testate bolle'
054300990915     O          E            riepilogo   2
054400990915     O                       wrkdata
054500990915     O                       wrkora              +1
054600990915     O                       procname            +1
054700990915     O          E            riepilogo   2
054800990915     O                                              'Cliente..................:'
054900010605     O                       VABCCM        z     +1
055000990915     O          E            riepilogo   2
055100990920     O                                              'Riferimento Strategi.....:'
055200990920     O                       vlrhdl              +1
055300990915     O          E            riepilogo   2
055400990915     O                                              'Giusti...................:'
055500971022     O                       §CTROK        2   +  1
055600990915     O          E            riepilogo   2
055700990915     O                                              'Sbagliati e corretti.....:'
055800971022     O                       §CTRMO        2   +  1
055900990915     O          E            riepilogo   2
056000990915     O                                              'Sbagliati e scartati.....:'
056100971022     O                       §CTRNO        2   +  1
056200000613
056300010910     Otis7lyps  E            testdett          2
056400000613     O                                              'Upload via Internet'
056500000613     O                                           +1 'Traduzione TIVIN00R -'
056600021113     O                                           +1 'FIVABWWR'
056700000613     O                                           +1 'Testate bolle'
056800000616     O          E            testdett    3
056900000613     O                                           +2 'N° rec'
057000000613     O                                           +3 'Anteprima contenuto'
057100000616     O          E            rigadett    2
057200000613     O                       rrnum               +2
057300000621     O                       recko               +3
057400000616     O          E            findett     2
057500000613     O                       wrkdata
057600000613     O                       wrkora              +1
057700000613     O                       procname            +1
057800000616     O          E            findett     2
057900000613     O                                              'Cliente..................:'
058000010605     O                       VABCCM        z     +1
058100000616     O          E            findett     2
058200000613     O                                              'Riferimento Strategi.....:'
058300000613     O                       vlrhdl              +1
058400000616     O          E            findett     2
058500000613     O                                              'Giusti...................:'
058600000613     O                       §CTROK        2   +  1
058700000616     O          E            findett     2
058800000613     O                                              'Sbagliati e corretti.....:'
058900000613     O                       §CTRMO        2   +  1
059000000616     O          E            findett     2
059100000613     O                                              'Sbagliati e scartati.....:'
059200000613     O                       §CTRNO        2   +  1
059300000613** CMD - COMANDI CL
059400011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7LYP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
059500011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7LYPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
059600010910DLTOVR FILE(TIS7LYP TIS7LYPS) LVL(*)
059700010606
059800000613
