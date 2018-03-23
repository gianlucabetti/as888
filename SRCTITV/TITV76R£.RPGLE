000100031124      /TITLE Upload via Internet: traduzione in FIVABWWR. (x AUTOTRASP. SALTI - Cod. 0930266)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700031124     Ftitv76p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900031124     Ftitv76ps  O    f  198        PRINTER usropn
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
004000000313     D*
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
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
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
009500031124     C                   if        not %open(titv76ps)
009600031124     C                   open      titv76ps
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
011800031124     C                   if        %open(titv76ps)
011900000616     C                   except    findett
012000031124     C                   close     titv76ps
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
014100031124     C                   if        not %open(titv76p)
014200031124     C                   open      titv76p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700031124     C                   if        %open(titv76p)
014800031124     C                   close     titv76p
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
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
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
020400021025     C                   if        vlrpoi = 999
020500021025     C                   MOVE(P)   invfil        VABFGS
020600021025     C                   else
020700021025     C                   MOVE(P)   vlrpoi        VABFGS
020800021025     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021025     C  N31              WRITE     FIVAB000
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
026700021025     C                   if        %open(fivabwwr)
026800021025     C                   close     fivabwwr
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
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010601
028700010601
028800010601
028900010330     C*----------------------------------------------------*
029000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100010330     C*----------------------------------------------------*
029200010330     C     INZVAR        BEGSR
029300010330     C*
029400020204     C                   Z-ADD     *zeros        Num5_0            5 0
029500030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029600030606     C                   MOVEL     '0'           FlgCAS            1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400020204     C* Inizializzo il buffer del record da scrivere
030500021025     C                   CLEAR                   FIVAB000
030600020204     C* Imposto i valori di default...
030700031124     C                   EVAL      VABCCM = 0930266
030800031124     C                   EVAL      VABLNP = 093
030900031013     C                   EVAL      VABCTR = 000
031000031124     C                   EVAL      VABCTM = '7K'
031001070330     C                   EVAL      VABTSP = 'C'
031002070330     C                   EVAL      VABCBO = '1'
031100020204     C* ... e poi verifico se sono stati passati come parametri
031200020204     C                   IF        vlrppt > *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABCCM
031700020204     C                   ENDIF
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABLNP
032200020204     C                   ENDIF
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABCTR
032700020204     C                   ENDIF
032800020204     C                   ENDIF
032900020204     C*
033000010330     C                   ENDSR
033100010607     C*----------------------------------------------------*
033200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033300010607     C*----------------------------------------------------*
033400010607     C     IMPVAB        BEGSR
033500010607     C*
033600010607     C                   SETOFF                                       3132
033700030515     C*
033800010607     C                   MOVEL     datcor        VABAAS
033900010607     C                   MOVE      datcor        VABMGS
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400070330     C                   EVAL      VABRSD=%trim(%subst(vindta:243:30))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800070330     C                   EVAL      VABRMO=%trim(%subst(vindta:53:25))
034900070330     C                   EVAL      VABIND=%trim(%subst(vindta:273:30))
035000070330     C                   EVAL      VABLOD=%trim(%subst(vindta:303:30))
035100070330     C                   EVAL      VABPRD=%trim(%subst(vindta:333:2))
035300070330     C                   EVAL      VABRMA=%trim(%subst(vindta:7:7))
035400070330     C                   EVAL      VABNOT=%trim(%subst(vindta:550:30))
035401070330     C                   EVAL      VABNT2=%trim(%subst(vindta:580:30))
035402070424     C                   IF        %trim(%subst(vindta:533:1)) = 'T'
035403070424     C                   EVAL      VABTCR='P'
035404070424     C                   ENDIF
035405070424     C                   IF        %trim(%subst(vindta:612:1)) = '1'
035406070424     C                   EVAL      VABTC1='A'
035407070424     C                   ENDIF
035500031124     C*
035600031124     C* Effettuo considerazioin sulla nazione destinatario
035700070424     C                   IF        %trim(%subst(vindta:340:3)) = '   '          * ITALIA
035800070424     C                   EVAL      VABNZD = *blanks
035900070424     C                   ENDIF
036000070424     C                   IF        %trim(%subst(vindta:340:3)) = 'UST'          * AUSTRIA
036100070424     C                   EVAL      VABNZD = 'A'
036200070424     C                   ENDIF
036300070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ANI'          * DANIMARCA
036400070424     C                   EVAL      VABNZD = 'DK'
036500070424     C                   ENDIF
036600070424     C                   IF        %trim(%subst(vindta:340:3)) = 'NGH'          * GRAN BRETAGNA
036700070424     C                   EVAL      VABNZD = 'GB'
036800070424     C                   ENDIF
036900070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ELG'          * BELGIO
037000070424     C                   EVAL      VABNZD = 'B'
037100070424     C                   ENDIF
037200070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ERM'          * GERMANIA
037300070424     C                   EVAL      VABNZD = 'D'
037400070424     C                   ENDIF
037500070424     C                   IF        %trim(%subst(vindta:340:3)) = 'PAG'          * SPAGNA
037600070424     C                   EVAL      VABNZD = 'E'
037700070424     C                   ENDIF
037800070424     C                   IF        %trim(%subst(vindta:340:3)) = 'RAN'          * FRANCIA
037900070424     C                   EVAL      VABNZD = 'F'
038000070424     C                   ENDIF
038100070424     C                   IF        %trim(%subst(vindta:340:3)) = 'USS'          * LUSSEMBURGO
038200070424     C                   EVAL      VABNZD = 'L'
038300070424     C                   ENDIF
038400070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ORT'          * PORTOGALLO
038500070424     C                   EVAL      VABNZD = 'P'
038600070424     C                   ENDIF
039000070424     C                   IF        %trim(%subst(vindta:340:3)) = 'LAN'          * OLANDA
039100070424     C                   EVAL      VABNZD = 'NL'
039200070424     C                   ENDIF
039201070424     C                   IF        %trim(%subst(vindta:340:3)) = 'Z'            * REP. CECA
039202070424     C                   EVAL      VABNZD = 'CZ'
039203070424     C                   ENDIF
039204070424     C                   IF        %trim(%subst(vindta:340:3)) = 'VEZ'          * SVEZIA
039205070424     C                   EVAL      VABNZD = 'S'
039206070424     C                   ENDIF
039207070424     C                   IF        %trim(%subst(vindta:340:3)) = 'VIZ'          * SVIZZERA
039208070424     C                   EVAL      VABNZD = 'CH'
039209070424     C                   ENDIF
039210070424     C*
039300031124     C* In relazione alla nazione destinatario modifico la tariffa
039400070424     C                   IF        VABNZD <> *blanks
039500070424     C                   EVAL      VABCTR = 001
039600070424     C                   ENDIF
039700010607     C*
039800010607     C* Reperimento campi NUMERICI
039900020604     C*
039901070424     C* DCR
039902070424     C                   IF        %subst(vindta:517:8) <> *all'0'
039903070424     C                   EVAL      PiStr=%trim(%subst(vindta:517:8))
039904070424     C                   EXSR      CHKNUM
039905070424     C                   IF        PiInt=*on
039906070424     C                   Z-ADD     PiVal         VABDCR
039907070424     C                   ELSE
039908070424     C                   SETON                                        32
039909070424     C                   EVAL      VABDCR = 1
039910070424     C                   EVAL      vinmsg = %trimr(vinmsg)
039911070424     C                             + ' ' + 'VABDCR'
039912070424     C                   ENDIF
039913070424     C                   ENDIF
040000031029     C* CAD
040100070330     C                   EVAL      PiStr=%trim(%subst(vindta:343:10))
040200010607     C                   EXSR      CHKNUM
040300010607     C                   IF        PiInt=*on
040400031124     C                   Z-ADD     PiVal         Num5_0
040500031124     C                   MOVEL(p)  Num5_0        VABCAD
040600010607     C                   ELSE
040700010607     C                   SETON                                        32
040800020204     C                   EVAL      VABCAD = *zeros
040900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041000020204     C                             + ' ' + 'VABCAD'
041100010607     C                   ENDIF
041200030515     C* RMN/NSP
041300070330     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
041400011023     C                   EXSR      CHKNUM
041500011023     C                   IF        PiInt=*on
041600030225     C                   Z-ADD     PiVal         VABRMN
041700031013     C                   MOVE(P)   VABRMN        VABNSP
041800011023     C                   ELSE
041900020205     C                   EVAL      VABRMN = 1
042000011023     C                   ENDIF
042100030515     C* NCL
042200070330     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
042300010607     C                   EXSR      CHKNUM
042400010607     C                   IF        PiInt=*on
042500010607     C                   Z-ADD     PiVal         VABNCL
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABNCL
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABNCL'
043100010607     C                   ENDIF
043200031013     C* PKB
043300070330     C                   EVAL      PiStr=%trim(%subst(vindta:378:8))
043400031013     C                   EXSR      CHKNUM
043500031017     C                   IF        PiNum=*on
043600070330     C                   EVAL      VABPKB = PiVal/1000                          * gestisco 3 decim.
043700031013     C                   ELSE
043800031013     C                   SETON                                        32
043900031013     C                   Z-ADD     *zeros        VABPKB
044000031013     C                   EVAL      vinmsg = %trimr(vinmsg)
044100031013     C                             + ' ' + 'VABPKB'
044200031013     C                   ENDIF
045400031124     C* CAS
045500070330     C                   IF        %subst(vindta:413:13) <> '0000000000000'
045600031124     C                   EVAL      FlgCAS = '1'
045700070330     C                   EVAL      VABVCA=%trim(%subst(vindta:426:3))
045701070330     C*
045702070330     C                   IF        %trim(%subst(vindta:623:3))='001'
045800070330     C                   EVAL      VABTIC=*blanks
045801070330     C                   ENDIF
045802070330     C                   IF        %trim(%subst(vindta:623:3))='002'
045803070330     C                   EVAL      VABTIC='OM'
045804070330     C                   ENDIF
045805070330     C                   IF        %trim(%subst(vindta:623:3))='003'
045806070330     C                   EVAL      VABTIC='CM'
045807070330     C                   ENDIF
045808070330     C                   IF        %trim(%subst(vindta:623:3))='004'
045809070330     C                   EVAL      VABTIC='BM'
045810070330     C                   ENDIF
045811070330     C*
045900070330     C                   EVAL      PiStr=%trim(%subst(vindta:413:13))
046000031124     C                   EXSR      CHKNUM
046100031124     C                   IF        PiNum=*on
046200070330     C                   EVAL      VABCAS = PiVal/10000                         * gestisco 4 decim.
046300031124     C                   ELSE
046400031124     C                   SETON                                        32
046500031124     C                   Z-ADD     *zeros        VABCAS
046600031124     C                   EVAL      vinmsg = %trimr(vinmsg)
046700031124     C                             + ' ' + 'VABCAS'
046800031124     C                   ENDIF
046900031124     C                   ENDIF
047000030929     C*
047100030929     C* Considerazioni finali su CBO/CAS
047200030929     C                   IF        FlgCAS = '1'
047300030929     C                   EVAL      VABCBO = '4'
047400030929     C                   ELSE
047500030929     C                   EVAL      VABCBO = '1'
047600030929     C                   ENDIF
047700020204     C*
047800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047900020204     C                   EXSR      CHKIMPDIV
048000020204     C*
048100010607     C                   ENDSR
048200010607     C*----------------------------------------------------*
048300010601
048400020204
048500020204     C*----------------------------------------------------*
048600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048700020204     C*----------------------------------------------------*
048800020204     C     CHKIMPDIV     BEGSR
048900020204     C*
049000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049100020204     C                   Z-ADD     *zeros        wrkDec            9 9
049200020204     C*
049300020204     C* Come prima cosa effettuo considerazioni sulla divisa
049400020204     C                   IF        vabIAS > *zeros
049500020204     C                   IF        vabVAS <> 'EUR'
049600020204     C                   EVAL      vabVAS =  'ITL'
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C                   IF        vabCAS > *zeros
050100020204     C                   IF        vabVCA <> 'EUR'
050200020204     C                   EVAL      vabVCA =  'ITL'
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C                   IF        vabVMD > *zeros
050700020204     C                   IF        vabVAD <> 'EUR'
050800020204     C                   EVAL      vabVAD =  'ITL'
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051300020204     C                   Z-ADD     vabIAS        wrkDec
051400020204     C                   IF        wrkDec > *zeros
051500020204     C                   IF        vabVAS = 'ITL'
051600020204     C                   EVAL      vabIAS = *zeros
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052100020204     C                   Z-ADD     vabCAS        wrkDec
052200020204     C                   IF        wrkDec > *zeros
052300020204     C                   IF        vabVCA = 'ITL'
052400020204     C                   EVAL      vabCAS = *zeros
052500020204     C                   ENDIF
052600020204     C                   ENDIF
052700020204     C*
052800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052900020204     C                   Z-ADD     vabVMD        wrkDec
053000020204     C                   IF        wrkDec > *zeros
053100020204     C                   IF        vabVAD = 'ITL'
053200020204     C                   EVAL      vabVMD = *zeros
053300020204     C                   ENDIF
053400020204     C                   ENDIF
053500020204     C*
053600020204     C                   ENDSR
053700020204     C***
053800020204
053900010330
054000010330
054100010330     C*----------------------------------------------------*
054200010330     C*  CONTROLLO NUMERICITA' CAMPI
054300010330     C*----------------------------------------------------*
054400010330     C     CHKNUM        BEGSR
054500010330     C*
054600010606     C                   IF        PiDecChr = *blanks
054700030606     C                   EVAL      PiDecChr = ','
054800010606     C                   ENDIF
054900010606     C*
055000010606     C                   CALL(e)   'ISNUMERIC'
055100010330     C                   PARM                    PiStr            30
055200010606     C                   PARM                    PiDecChr          1
055300010330     C                   PARM      *ZEROS        PiVal            30 9
055400010330     C                   PARM      '0'           PiInt             1
055500010330     C                   PARM      '0'           PiNum             1
055600010330     C                   IF        %error
055700010606     C                   EVAL      PiNum=*off
055800010330     C                   ENDIF
055900010330     C*
056000010330     C                   ENDSR
056100010330     C***
056200010330
056300010601
056400010601
056500010601
056600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056700010601     C     repfil        BEGSR
056800010601     C*
056900010601     C                   if        invfil = *zeros and
057000010601     C                             depfil > *zeros and
057100010629     C                             (vinflg = *blanks or
057200010629     C                              vinflg = *zeros)
057300010601     C
057400010601     C                   eval      invfil = depfil
057500010601     C                   endif
057600010601     C*
057700010601     C                   if        depfil <> invfil and
057800010601     C                             invfil > *zeros
057900010601     C                   eval      flgMulti = '1'
058000010601     C                   if        vinflg = *blanks
058100010601     C                   add       1             cntNonEl
058200010601     C                   endif
058300010601     C                   endif
058400010601     C*
058500010601     C                   if        vinflg = '2'
058600010601     C                   eval      flgStato = '2'
058700010601     C                   endif
058800010601     C*
058900010601     C                   ENDSR
059000010601     C***
059100010601
059200010601
059300010601
059400010330
059500010330
059600010330
059700990920      /TITLE Invio dei dati al punto operativo.
059800000613     C     invio         BEGSR
059900990920     C*
060000990920     C                   reset                   dscmz
060100010601     C                   move      invfil        cmzdst
060200990920     C                   eval      cmzfld = vlrfou
060300990920     C                   eval      cmzmbd = vlrhdl
060400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500000710     C                   if        prmfir = *blanks
060600021025     C                   eval      cmzfla = 'FIVAB00F'
060700021025     C                   eval      cmzmba = 'FIVAB00F'
060800000710     C                   else
060900000710     C                   eval      cmzfla = prmfir
061000000710     C                   eval      cmzmba = prmfir
061100000710     C                   endif
061200990920     C                   eval      cmznrr = *zeros
061300990920     C                   move      §ctrok        cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500990920     C                   call(e)   'TIS711C'
061600990920     C                   parm                    dscmz
061700990921     C                   parm      *blanks       esito
061800990920     C                   if        %error
061900990920     C                             or cmzerr = '1'
062000990921     C                             or esito  = '1'
062100000710     C                   eval      wrkesito = '3'
062200990920     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500990910
062600010601
062700010601
062800010601
062900010601
063000010601      /TITLE Invio dei dati al punto operativo.
063100010601     C     opeini        BEGSR
063200010601     C*
063300010601     C* Inizializzo flag e contatori operativi
063400010601     C                   movel     '0'           flgGiro           1
063500010601     C                   movel     '0'           flgMulti          1
063600010601     C                   movel     '1'           flgStato          1
063700010615     C                   movel     '0'           flgOk             1
063800010601     C                   z-add     *zeros        cntNonEl         10 0
063900010601     C                   z-add     *zeros        depfil            3 0
064000010601     C                   z-add     *zeros        invfil            3 0
064100010601     C*
064200010601     C                   ENDSR
064300010601     C***
064400010601
064500010601
064600010601
064700010330
064800010330
064900000613     C     *inzsr        BEGSR
065000990910     C*
065100990910     C     *entry        plist
065200990920     C                   parm                    tivlrds
065300990921     C                   parm      wrkesito      esito
065400000724     C                   parm                    prmlit
065500000710     C                   parm                    prmfir
065600010330     C*
065700010330     C* CALCOLA LA DATA CORRENTE
065800010330     C                   time                    wn14             14 0
065900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066000010330     C                   z-add     wn8           g08dat
066100010330     C                   z-add     *zeros        g08inv
066200010330     C                   movel     '0'           g08err
066300010330     C                   call      'XSRDA8'
066400010330     C                   parm                    wlbda8
066500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066600000613     C*
066700000613     C                   ENDSR
066800000613     C***
066900990908
067000031124     Otitv76p   E            riepilogo         2
067100990915     O                                              'Upload via Internet'
067200990915     O                                           +1 'Traduzione TIVIN00R -'
067300021025     O                                           +1 'FIVABWWR'
067400990915     O                                           +1 'Testate bolle'
067500990915     O          E            riepilogo   2
067600990915     O                       wrkdata
067700990915     O                       wrkora              +1
067800990915     O                       procname            +1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Cliente..................:'
068100010605     O                       VABCCM        z     +1
068200990915     O          E            riepilogo   2
068300990920     O                                              'Riferimento Strategi.....:'
068400990920     O                       vlrhdl              +1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Giusti...................:'
068700971022     O                       §CTROK        2   +  1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Sbagliati e corretti.....:'
069000971022     O                       §CTRMO        2   +  1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Sbagliati e scartati.....:'
069300971022     O                       §CTRNO        2   +  1
069400000613
069500031124     Otitv76ps  E            testdett          2
069600000613     O                                              'Upload via Internet'
069700000613     O                                           +1 'Traduzione TIVIN00R -'
069800021025     O                                           +1 'FIVABWWR'
069900000613     O                                           +1 'Testate bolle'
070000000616     O          E            testdett    3
070100000613     O                                           +2 'N° rec'
070200000613     O                                           +3 'Anteprima contenuto'
070300000616     O          E            rigadett    2
070400000613     O                       rrnum               +2
070500000621     O                       recko               +3
070600000616     O          E            findett     2
070700000613     O                       wrkdata
070800000613     O                       wrkora              +1
070900000613     O                       procname            +1
071000000616     O          E            findett     2
071100000613     O                                              'Cliente..................:'
071200010605     O                       VABCCM        z     +1
071300000616     O          E            findett     2
071400000613     O                                              'Riferimento Strategi.....:'
071500000613     O                       vlrhdl              +1
071600000616     O          E            findett     2
071700000613     O                                              'Giusti...................:'
071800000613     O                       §CTROK        2   +  1
071900000616     O          E            findett     2
072000000613     O                                              'Sbagliati e corretti.....:'
072100000613     O                       §CTRMO        2   +  1
072200000616     O          E            findett     2
072300000613     O                                              'Sbagliati e scartati.....:'
072400000613     O                       §CTRNO        2   +  1
072500000613** CMD - COMANDI CL
072600031124OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV76P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072700031124OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV76PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072800031124DLTOVR FILE(TITV76P TITV76PS) LVL(*)
072900010606
073000000613
