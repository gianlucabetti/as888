000100070213      /TITLE Upload via Internet: traduzione in FIVABWWR. (SEM EXPRESS - Cod. 0052973)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700050209     FFIVATwwr  O    E             DISK    usropn
000800070214     Ftitvy0p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070214     Ftitvy0ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900990915     D wrkora          s               t
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100000621     D recko           s            150    INZ(*blanks)
003200011123     D depcmd          s            150    INZ(*blanks)
003300050209     D parccm          s              8    INZ(*blanks)
003400050209     D parmbr          s             10    INZ(*blanks)
003500050209     D paropz          s              1    INZ(*blanks)
003600050209     D chkcall         s              1    INZ(*blanks)
003700070214     D*------------------
003800070214     D* DS X RIDEFINIZIONE "ORIZZONTALE" BARCODE COLLI SINGOLA SPEDIZIONE
003900070214     D*------------------
004000070214     D DSWHOIS         DS
004100070214     D  WHOIS01                      20
004200070214     D  FILL001                       1
004300070214     D  WHOIS02                      20
004400070214     D  FILL002                       1
004500070214     D  WHOIS03                      20
004600070214     D  FILL003                       1
004700070214     D  WHOIS04                      20
004800070214     D  FILL004                       1
004900070214     D  WHOIS05                      20
005000070214     D  FILL005                       1
005100070214     D  WHOIS06                      20
005200070214     D  FILL006                       1
005300070214     D  WHOIS07                      20
005400070214     D  FILL007                       1
005500070214     D  WHOIS08                      20
005600070214     D  FILL008                       1
005700070214     D  WHOIS09                      20
005800070214     D  FILL009                       1
005900070214     D  WHOIS10                      20
006000070214     D  FILL010                       1
006100070214     D  WHOIS11                      20
006200070214     D  FILL011                       1
006300070214     D  WHOIS12                      20
006400070214     D  FILL012                       1
006500070214     D  WHOIS13                      20
006600070214     D  FILL013                       1
006700070214     D  WHOIS14                      20
006800070214     D  FILL014                       1
006900070214     D  WHOIS15                      20
007000070214     D  FILL015                       1
007100070214     D  WHOIS16                      20
007200070214     D  FILL016                       1
007300070214     D  WHOIS17                      20
007400070214     D  FILL017                       1
007500070214     D  WHOIS18                      20
007600070214     D  FILL018                       1
007700070214     D  WHOIS19                      20
007800070214     D  FILL019                       1
007900070214     D  WHOIS20                      20
008000070214     D  FILL020                       1
008100010606     D*------------------
008200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300010606     D*------------------
008400010606     D WLBDA8          DS                  INZ
008500010606     D  G08DAT                 1      8  0
008600010606     D  G08INV                 9     16  0
008700010606     D  G08ERR                17     17
008800010606     D  G08TGI                18     22  0
008900050209     D*------------------
009000050209     D* DS ARCHITETTURA
009100050209     D*------------------
009200050209     D kpjba         e ds                  inz
009300050209     D*------------------
009400990908
009500050209
009600050209
009700050209     C* /MAIN
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010530     C*
010400010530     C                   exsr      opeini
010500010530     C                   exsr      rwvab
010600050209     C*
010700050209     C* Effettuo la chiamata al CLLE preposto
010800050209     C                   call(e)   'TITVVTC'
010900050209     C                   parm                    parccm
011000050209     C                   parm                    parmbr
011100050209     C                   parm      '2'           paropz
011200010530     C*
011300010530     C                   seton                                        lr
011400010530
011500010530
011600010530
011700010530
011800010530     C*--------------------------------------------------------
011900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010530     C*--------------------------------------------------------
012100010530     C     PREELA        BEGSR
012200000724     C*
012300000724     C* SE OCCORRE SPEDIRE IN FILIALE
012400010530     C                   if        invfil <> *zeros and
012500010530     C                             flgGiro = '0'
012600010530     C*
012700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010530     C                   eval      flgGiro = '1'
012900000724     C*
013000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200010530     C     invfil        chain     azorg01l
013300000724     C                   if        %found
013400000616     C                   movel(p)  CMD(1)        depcmd
013500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013600000616     C*
013700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013800011123     C                   Z-ADD     150           LENGH            15 5
013900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014000000616     C                   PARM                    depcmd
014100000616     C                   PARM                    LENGH
014200000724     C*
014300000724     C                   endif
014400000616     C*
014500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014600000616     C                   movel(p)  CMD(2)        depcmd
014700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014800000616     C*
014900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015000011123     C                   Z-ADD     150           LENGH            15 5
015100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015200000616     C                   PARM                    depcmd
015300000616     C                   PARM                    LENGH
015400000616     C*
015500070214     C                   if        not %open(titvy0ps)
015600070214     C                   open      titvy0ps
015700030407     C   50              except    testdett
015800000613     C                   endif
015900010530     C*
016000010530     C                   endif
016100010530     C*
016200010530     C                   ENDSR
016300010530     C***
016400010530
016500010530
016600010530
016700010530
016800010530
016900010530
017000010530     C*--------------------------------------------------------
017100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017200010530     C*--------------------------------------------------------
017300010530     C     ENDELA        BEGSR
017400010530     C*
017500010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017600000613     C*
017700070214     C                   if        %open(titvy0ps)
017800030407     C   50              except    findett
017900070214     C                   close     titvy0ps
018000000613     C                   endif
018100000616     C*
018200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018300011123     C                   Z-ADD     150           LENGH            15 5
018400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018500000616     C                   PARM                    CMD(3)
018600000616     C                   PARM                    LENGH
018700010530     C*
018800010530     C                   ENDSR
018900010530     C***
019000000613
019100010530
019200010530
019300000613
019400000613     C*--------------------------------------------------------
019500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019600000613     C*--------------------------------------------------------
019700000621     C     STPR          BEGSR
019800000613     C*
019900070214     C                   if        not %open(titvy0p)
020000070214     C                   open      titvy0p
020100990915     C                   endif
020200990915     C*
020300030407     C   50              except    riepilogo
020400990915     C*
020500070214     C                   if        %open(titvy0p)
020600070214     C                   close     titvy0p
020700990914     C                   endif
020800990910     C*
020900000613     C                   ENDSR
021000000613     C***
021100070214
021200070214
021300070214
021400070214     C*--------------------------------------------------------
021500070214     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
021600070214     C*--------------------------------------------------------
021700070214     C     RWVAB         BEGSR
021800070214     C*
021900070214     C                   if        not %open(tivin00r)
022000070214     C                   open      tivin00r
022100070214     C                   endif
022200070214     C                   if        not %open(fivabwwr)
022300070214     C                   open      fivabwwr
022400070214     C                   endif
022500070214     C*
022600070214     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022700070214     C                   exsr      prevat
022800070214     C*
022900070214     C                   if        chkcall = '0'
023000070214     C*
023100070214     C                   if        not %open(fivatwwr)
023200070214     C                   open      fivatwwr
023300070214     C                   endif
023400070214     C*
023500070214     C                   clear                   §CTROK
023600070214     C                   clear                   §CTRMO
023700070214     C                   clear                   §CTRNO
023800070214     C*
023900070214     C                   DO        *HIVAL
024000070214     C*
024100070214     C                   READ      tivin00r                               70
024200070214     C*
024300070214     C* Dopo ogni lettura verifico se ci sono stati record OK
024400070214     C                   if        vinflg = '1'
024500070214     C                   eval      flgOk = '1'
024600070214     C                   endif
024700070214     C*
024800070214     C                   if        vindta > *blanks
024900070214     C                   add       1             rrnum
025000070214     C*
025100070214     C                   if        *in70 = *off and
025200070214     C                             (vinflg = *blanks
025300070214     C                              or vinflg = '0'
025400070214     C                              or vinflg = '2')
025500070214     C*
025600070214     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025700070214     C                   if        vinflg = *blanks or vinflg = '0'
025800070214     C                   clear                   vinmsg
025900070214     C                   endif
026000070214     C*
026100070214     C                   exsr      inzvar
026200070214     C                   exsr      defcam
026300070214     C                   exsr      impvab
026400070214     C*
026500070214     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600070214     C                   eval      depfil = VABLNP
026700070214     C                   exsr      repfil
026800070214     C                   if        depfil = invfil
026900070214     C                   if        vlrpoi = 999
027000070214     C                   MOVE(P)   invfil        VABFGS
027100070214     C                   else
027200070214     C                   MOVE(P)   vlrpoi        VABFGS
027300070214     C                   endif
027400070214     C*
027500070214     C                   exsr      PREELA
027600070214     C*
027700070214     C* Ebbene...
027800070214     C*
027900070214     C  N31              ADD       1             §CTROK            7 0
028000070214     C   32              ADD       1             §CTRMO            7 0
028100070214     C   31              ADD       1             §CTRNO            7 0
028200070214     C*
028300070214     C* Scarico il buffer del FIVAB
028400070214     C  N31              WRITE     FIVAB000
028500070214     C  N31              EXSR      WRIVAT
028600070214     C*
028700070214     C                   endif
028800070214     C*
028900070214     C                   if        *in31 = *off and
029000070214     C                             *in32 = *off
029100070214     C                   eval      vinflg = '1'
029200070214     C                   else
029300070214     C                   eval      recko = vindta
029400070214     C                   except    rigadett
029500070214     C                   eval      vinflg = '2'
029600070214     C                   endif
029700070214     C                   endif
029800070214     C*
029900070214     C                   else
030000070214     C                   eval      vinflg = '1'
030100070214     C                   endif
030200070214     C*
030300070214     C  N70              update    tivin000
030400070214     C*
030500070214     C  N70              ENDdo
030600070214     C*
030700070214     C                   endif                                                  <= prevat
030800070214     C*
030900070214     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031000070214     C                   if        cntNonEl = *zeros or
031100070214     C                             flgMulti = '0'
031200070214     C* Se non ci sono record con errori ...
031300070214     C                   if        §ctrno = 0 and
031400070214     C                             §ctrmo = 0 and
031500070214     C                             flgStato <> '2'
031600070214     C* ... restituisco esito OK.
031700070214     C                   eval      wrkesito = '0'
031800070214     C                   else
031900070214     C                   if        §ctrok > 0
032000070214     C                   eval      wrkesito = '1'
032100070214     C                   else
032200070214     C                   if        flgOk = '0'
032300070214     C                   eval      wrkesito = '2'
032400070214     C                   else
032500070214     C                   eval      wrkesito = '6'
032600070214     C                   endif
032700070214     C                   endif
032800070214     C                   endif
032900070214     C                   else
033000070214     C                   eval      wrkesito = '9'
033100070214     C                   endif
033200070214     C*
033300070214     C                   if        %open(tivin00r)
033400070214     C                   close     tivin00r
033500070214     C                   endif
033600070214     C                   if        %open(fivabwwr)
033700070214     C                   close     fivabwwr
033800070214     C                   endif
033900070214     C                   if        %open(fivatwwr)
034000070214     C                   close     fivatwwr
034100070214     C                   endif
034200070214     C*
034300070214     C                   if        vlrpoi <> 999
034400070214     C                   eval      invfil = vlrpoi
034500070214     C                   endif
034600070214     C*
034700070214     C                   if        §ctrok > 0
034800070214     C                             and invfil > *zeros
034900070214     C                   exsr      invio
035000070214     C                   endif
035100070214     C*
035200070214     C                   if        flgGiro = '1'
035300070214     C                   exsr      endela
035400070214     C                   endif
035500070214     C*
035600070214     C                   ENDSR
035700070214     C***
035800050209
035900070214
036000070214
036100070214     C*----------------------------------------------------*
036200070214     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036300070214     C*----------------------------------------------------*
036400070214     C     INZVAR        BEGSR
036500070214     C*
036600070214     C                   Z-ADD     *zeros        Num5_0            5 0
036700070214     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036800070214     C                   MOVEL     '0'           FlgCAS            1
036900070214     C*
037000070214     C                   ENDSR
037100070214     C*----------------------------------------------------*
037200070214     C*  IMPOSTAZIONE CAMPI COSTANTI
037300070214     C*----------------------------------------------------*
037400070214     C     DEFCAM        BEGSR
037500070214     C*
037600070214     C* Inizializzo il buffer del record da scrivere
037700070214     C                   CLEAR                   FIVAB000
037800070214     C                   CLEAR                   FIVAT000
037900070214     C* Imposto i valori di default...
038000070214     C                   EVAL      VABCCM = 0593027
038100070214     C                   EVAL      VATCCM = 0593027
038200070214     C                   EVAL      VABLNP = 059
038300070214     C                   EVAL      VATLNP = 059
038400070214     C                   EVAL      VABCTR = 000
038500070214     C                   EVAL      VABCBO = '1'
038600070214     C                   EVAL      VABCTM = '7Q'
038700070214     C                   EVAL      VABTSP = 'C'
038800070214     C* ... e poi verifico se sono stati passati come parametri
038900070214     C                   IF        vlrppt > *blanks
039000070214     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
039100070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039200070214     C                   EXSR      CHKNUM
039300070214     C                   IF        PiInt=*on
039400070214     C                   Z-ADD     PiVal         VABCCM
039500070214     C                   Z-ADD     PiVal         VATCCM
039600070214     C                   ENDIF
039700070214     C                   ENDIF
039800070214     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
039900070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040000070214     C                   EXSR      CHKNUM
040100070214     C                   IF        PiInt=*on
040200070214     C                   Z-ADD     PiVal         VABLNP
040300070214     C                   Z-ADD     PiVal         VATLNP
040400070214     C                   ENDIF
040500070214     C                   ENDIF
040600070214     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
040700070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040800070214     C                   EXSR      CHKNUM
040900070214     C                   IF        PiInt=*on
041000070214     C                   Z-ADD     PiVal         VABCTR
041100070214     C                   ENDIF
041200070214     C                   ENDIF
041300070214     C                   ENDIF
041400070214     C*
041500070214     C                   ENDSR
041600070214     C*----------------------------------------------------*
041700070214     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041800070214     C*----------------------------------------------------*
041900070214     C     IMPVAB        BEGSR
042000070214     C*
042100070214     C                   SETOFF                                       3132
042200070214     C*
042300070214     C* Reperimento campi ALFA
042400070214     C*
042500070214     C* Considerazioni sulla ragione sociale del destinatario da indicare
042600071023     C                   EVAL      VABRSD=%trim(%subst(vindta:169:35))
042700070214     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042800070214     C     '@':'A'       XLATE     VABRSD        VABRSD
042900070214     C* ==
043000071023     C                   EVAL      VABIND=%trim(%subst(vindta:204:35))
043100071023     C                   EVAL      VABLOD=%trim(%subst(vindta:239:25))
043200071023     C                   EVAL      VABPRD=%trim(%subst(vindta:264:2))
043300070214     C                   EVAL      VABNAS=%trim(%subst(vindta:318:30))
043400070214     C                   EVAL      VABRMA=%trim(%subst(vindta:153:7))
043500070214     C*
043600070214     C* Reperimento campi NUMERICI
043700070214     C                   MOVEL     DATCOR        VABAAS
043800070214     C                   MOVEL     DATCOR        VATAAS
043900070214     C                   MOVE      DATCOR        VABMGS
044000070214     C* NSP/RMN
044100070214     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
044200070214     C                   EXSR      CHKNUM
044300070214     C                   IF        PiInt=*on
044400070214     C                   Z-ADD     PiVal         VABNSP
044500070214     C                   Z-ADD     PiVal         VATNSP
044600070214     C                   Z-ADD     PiVal         VABRMN
044700070214     C                   ELSE
044800070214     C                   SETON                                        31
044900070214     C                   Z-ADD     *zeros        VABNSP
045000070214     C                   Z-ADD     1             VABRMN
045100070214     C                   EVAL      vinmsg = %trimr(vinmsg)
045200070214     C                             + ' ' + 'VABNSP VATNSP VABRMN'
045300070214     C                   ENDIF
045400070214     C* CAD
045500071023     C                   EVAL      PiStr=%trim(%subst(vindta:266:9))
045600070214     C                   EXSR      CHKNUM
045700070214     C                   IF        PiInt=*on
045800070214     C                   Z-ADD     PiVal         Num5_0
045900070214     C                   MOVEL(p)  Num5_0        VABCAD
046000070214     C                   ELSE
046100070214     C                   SETON                                        32
046200070214     C                   EVAL      VABCAD = *zeros
046300070214     C                   EVAL      vinmsg = %trimr(vinmsg)
046400070214     C                             + ' ' + 'VABCAD'
046500070214     C                   ENDIF
046600070214     C* NCL
046700070214     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
046800070214     C                   EXSR      CHKNUM
046900070214     C                   IF        PiInt=*on
047000070214     C                   Z-ADD     PiVal         VABNCL
047100070214     C                   ELSE
047200070214     C                   SETON                                        32
047300070214     C                   Z-ADD     *zeros        VABNCL
047400070214     C                   EVAL      vinmsg = %trimr(vinmsg)
047500070214     C                             + ' ' + 'VABNCL'
047600070214     C                   ENDIF
047700070214     C* PKB
047800070214     C                   EVAL      PiStr=%trim(%subst(vindta:282:7))
047900070214     C                   EXSR      CHKNUM
048000070214     C                   IF        PiNum=*on
048100070214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
048200070214     C                   Z-ADD(H)  PiVal         VABPKB
048300070214     C                   ELSE
048400070214     C                   SETON                                        32
048500070214     C                   Z-ADD     *zeros        VABPKB
048600070214     C                   EVAL      vinmsg = %trimr(vinmsg)
048700070214     C                             + ' ' + 'VABPKB'
048800070214     C                   ENDIF
048900070214     C* VLB
049000070214     C                   EVAL      PiStr=%trim(%subst(vindta:275:7))
049100070214     C                   EXSR      CHKNUM
049200070214     C                   IF        PiNum=*on
049300070214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
049400070214     C                   Z-ADD(H)  PiVal         VABVLB
049500070214     C                   ELSE
049600070214     C                   SETON                                        32
049700070214     C                   Z-ADD     *zeros        VABVLB
049800070214     C                   EVAL      vinmsg = %trimr(vinmsg)
049900070214     C                             + ' ' + 'VABVLB'
050000070214     C                   ENDIF
050100070214     C*
050200070214     C* Considerazioni finali su CBO/CAS
050300070214     C                   IF        FlgCAS = '1'
050400070214     C                   IF        VABCBO = '1'
050500070214     C                   EVAL      VABCBO = '4'
050600070214     C                   ENDIF
050700070214     C                   IF        VABCBO = '2'
050800070214     C                   EVAL      VABCBO = '6'
050900070214     C                   ENDIF
051000070214     C                   ENDIF
051100070214     C*
051200070214     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051300070214     C                   EXSR      CHKIMPDIV
051400070214     C*
051500070214     C                   ENDSR
051600070214     C*----------------------------------------------------*
051700070214
051800070214
051900070214     C*----------------------------------------------------*
052000070214     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052100070214     C*----------------------------------------------------*
052200070214     C     CHKIMPDIV     BEGSR
052300070214     C*
052400070214     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052500070214     C                   Z-ADD     *zeros        wrkDec            9 9
052600070214     C*
052700070214     C* Come prima cosa effettuo considerazioni sulla divisa
052800070214     C                   IF        vabIAS > *zeros
052900070214     C                   IF        vabVAS <> 'EUR'
053000070214     C                   EVAL      vabVAS =  'ITL'
053100070214     C                   ENDIF
053200070214     C                   ENDIF
053300070214     C*
053400070214     C                   IF        vabCAS > *zeros
053500070214     C                   IF        vabVCA <> 'EUR'
053600070214     C                   EVAL      vabVCA =  'ITL'
053700070214     C                   ENDIF
053800070214     C                   ENDIF
053900070214     C*
054000070214     C                   IF        vabVMD > *zeros
054100070214     C                   IF        vabVAD <> 'EUR'
054200070214     C                   EVAL      vabVAD =  'ITL'
054300070214     C                   ENDIF
054400070214     C                   ENDIF
054500070214     C*
054600070214     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054700070214     C                   Z-ADD     vabIAS        wrkDec
054800070214     C                   IF        wrkDec > *zeros
054900070214     C                   IF        vabVAS = 'ITL'
055000070214     C                   EVAL      vabIAS = *zeros
055100070214     C                   ENDIF
055200070214     C                   ENDIF
055300070214     C*
055400070214     C* Stabilisco se il contrasegno ha decimali valorizzati
055500070214     C                   Z-ADD     vabCAS        wrkDec
055600070214     C                   IF        wrkDec > *zeros
055700070214     C                   IF        vabVCA = 'ITL'
055800070214     C                   EVAL      vabCAS = *zeros
055900070214     C                   ENDIF
056000070214     C                   ENDIF
056100070214     C*
056200070214     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056300070214     C                   Z-ADD     vabVMD        wrkDec
056400070214     C                   IF        wrkDec > *zeros
056500070214     C                   IF        vabVAD = 'ITL'
056600070214     C                   EVAL      vabVMD = *zeros
056700070214     C                   ENDIF
056800070214     C                   ENDIF
056900070214     C*
057000070214     C                   ENDSR
057100070214     C***
057200070214
057300070214
057400070214
057500070214     C*----------------------------------------------------*
057600070214     C*  CONTROLLO NUMERICITA' CAMPI
057700070214     C*----------------------------------------------------*
057800070214     C     CHKNUM        BEGSR
057900070214     C*
058000070214     C                   IF        PiDecChr = *blanks
058100070214     C                   EVAL      PiDecChr = '.'
058200070214     C                   ENDIF
058300070214     C*
058400070214     C                   CALL(e)   'ISNUMERIC'
058500070214     C                   PARM                    PiStr            30
058600070214     C                   PARM                    PiDecChr          1
058700070214     C                   PARM      *ZEROS        PiVal            30 9
058800070214     C                   PARM      '0'           PiInt             1
058900070214     C                   PARM      '0'           PiNum             1
059000070214     C                   IF        %error
059100070214     C                   EVAL      PiNum=*off
059200070214     C                   ENDIF
059300070214     C*
059400070214     C                   ENDSR
059500070214     C***
059600050209
059700050209
059800050209     C*----------------------------------------------------*
059900050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060000050209     C*----------------------------------------------------*
060100050209     C     PREVAT        BEGSR
060200050209     C*
060300050209     C* Compongo il nome del membro da dare al FIVATWWR
060400050209     C                   eval      parmbr = vlrhdl
060500050209     C                   movel     'M'           parmbr
060600050209     C                   eval      parccm = %subst(vlrKSC:2:7)
060700050209     C                   eval      paropz = '1'
060800050209     C* Effettuo la chiamata al CLLE preposto
060900050209     C                   call(e)   'TITVVTC'
061000050209     C                   parm                    parccm
061100050209     C                   parm                    parmbr
061200050209     C                   parm                    paropz
061300050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061400050209     C                   if        %error
061500050209     C                   movel     '1'           chkcall
061600050209     C                   else
061700050209     C                   movel     '0'           chkcall
061800050209     C                   endif
061900050209     C*
062000050209     C                   ENDSR
062100050209
062200050209
062300990920
062400050209     C*----------------------------------------------------*
062500050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
062600050209     C*----------------------------------------------------*
062700050209     C     WRIVAT        BEGSR
062800050209     C*
062900050209     C* Valorizzo il buffer del file dettagli barcode colli
063000050209     C                   EVAL      VATFGS = VABFGS
063100050209     C                   EVAL      VATAAS = VABAAS
063200050209     C                   EVAL      VATLNP = VABLNP
063300050209     C                   EVAL      VATNSP = VABNSP
063400050209     C                   EVAL      VATCCM = VABCCM
063500070209     C                   EVAL      VATTRC='E'
063600070214     C*
063700070214     C* Ridefinisco il campo con l'elenco orizzonatale dei "chi sono"
063800070214     C                   EVAL      DSWHOIS = %subst(vindta:360:512)
063900070214     C*
064000070214     C* Quindi scarico buffer x ogni id collo presente (max 10 colli x bolla)
064100070214     C                   IF        WHOIS01 <> *blanks
064200070214     C                   EVAL      VATNOT = WHOIS01
064300070214     C                   WRITE     FIVAT000
064400070214     C                   ENDIF
064500070214     C*
064600070214     C                   IF        WHOIS02 <> *blanks
064700070214     C                   EVAL      VATNOT = WHOIS02
064800070214     C                   WRITE     FIVAT000
064900070214     C                   ENDIF
065000070214     C*
065100070214     C                   IF        WHOIS03 <> *blanks
065200070214     C                   EVAL      VATNOT = WHOIS03
065300070214     C                   WRITE     FIVAT000
065400070214     C                   ENDIF
065500070214     C*
065600070214     C                   IF        WHOIS04 <> *blanks
065700070214     C                   EVAL      VATNOT = WHOIS04
065800070214     C                   WRITE     FIVAT000
065900070214     C                   ENDIF
066000070214     C*
066100070214     C                   IF        WHOIS05 <> *blanks
066200070214     C                   EVAL      VATNOT = WHOIS05
066300070214     C                   WRITE     FIVAT000
066400070214     C                   ENDIF
066500070214     C*
066600070214     C                   IF        WHOIS06 <> *blanks
066700070214     C                   EVAL      VATNOT = WHOIS06
066800070214     C                   WRITE     FIVAT000
066900070214     C                   ENDIF
067000070214     C*
067100070214     C                   IF        WHOIS07 <> *blanks
067200070214     C                   EVAL      VATNOT = WHOIS07
067300070214     C                   WRITE     FIVAT000
067400070214     C                   ENDIF
067500070214     C*
067600070214     C                   IF        WHOIS08 <> *blanks
067700070214     C                   EVAL      VATNOT = WHOIS08
067800070214     C                   WRITE     FIVAT000
067900070214     C                   ENDIF
068000070214     C*
068100070214     C                   IF        WHOIS09 <> *blanks
068200070214     C                   EVAL      VATNOT = WHOIS09
068300070214     C                   WRITE     FIVAT000
068400070214     C                   ENDIF
068500070214     C*
068600070214     C                   IF        WHOIS10 <> *blanks
068700070214     C                   EVAL      VATNOT = WHOIS10
068800070214     C                   WRITE     FIVAT000
068900070214     C                   ENDIF
069000050209     C*
069100050209     C                   ENDSR
069200010530
069300010530
069400050209
069500010530
069600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
069700010530     C     repfil        BEGSR
069800010530     C*
069900010530     C                   if        invfil = *zeros and
070000010530     C                             depfil > *zeros and
070100010613     C                             (vinflg = *blanks or
070200010613     C                              vinflg = *zeros)
070300010530     C
070400010530     C                   eval      invfil = depfil
070500010530     C                   endif
070600010530     C*
070700010530     C                   if        depfil <> invfil and
070800010530     C                             invfil > *zeros
070900010530     C                   eval      flgMulti = '1'
071000010530     C                   if        vinflg = *blanks
071100010530     C                   add       1             cntNonEl
071200010530     C                   endif
071300010530     C                   endif
071400010530     C*
071500010530     C                   if        vinflg = '2'
071600010530     C                   eval      flgStato = '2'
071700010530     C                   endif
071800010530     C*
071900010530     C                   ENDSR
072000010530     C***
072100050209
072200050209
072300050209
072400050209
072500050209      /TITLE Invio dei dati al punto operativo.
072600050209     C     invio         BEGSR
072700050209     C*
072800050209     C* 1° invio FIVAT
072900050209     C                   reset                   dscmz
073000050209     C                   move      vlrpoi        cmzdst
073100050209     C                   eval      cmzfld = 'FIVATWWR'
073200050209     C                   eval      cmzmbd = vlrhdl
073300050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
073400050209     C***                if        prmfir = *blanks
073500050209     C                   eval      cmzfla = 'FIVAT00F'
073600050209     C                   eval      cmzmba = 'FIVAT00F'
073700050209     C***                else
073800050209     C***                eval      cmzfla = prmfir
073900050209     C***                eval      cmzmba = prmfir
074000050209     C***                endif
074100050209     C                   eval      cmznrr = *zeros
074200050209     C                   move      §ctrok        cmznrr
074300050209     C                   eval      cmzlba = vlrfl1
074400050209     C                   call(e)   'TIS711C'
074500050209     C                   parm                    dscmz
074600050209     C                   parm      *blanks       esito
074700050209     C                   if        %error
074800050209     C                             or cmzerr = '1'
074900050209     C                             or esito  = '1'
075000050209     C                   eval      wrkesito = '3'
075100050209     C                   else
075200050209     C*
075300050209     C* 2° invio FIVAB
075400050209     C                   reset                   dscmz
075500050209     C                   move      vlrpoi        cmzdst
075600050209     C                   eval      cmzfld = vlrfou
075700050209     C                   eval      cmzmbd = vlrhdl
075800050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
075900050209     C***                if        prmfir = *blanks
076000050209     C                   eval      cmzfla = 'FIVAB00F'
076100050209     C                   eval      cmzmba = 'FIVAB00F'
076200050209     C***                else
076300050209     C***                eval      cmzfla = prmfir
076400050209     C***                eval      cmzmba = prmfir
076500050209     C***                endif
076600050209     C                   eval      cmznrr = *zeros
076700050209     C                   move      §ctrok        cmznrr
076800050209     C                   eval      cmzlba = vlrfl1
076900050209     C                   call(e)   'TIS711C'
077000050209     C                   parm                    dscmz
077100050209     C                   parm      *blanks       esito
077200050209     C                   if        %error
077300050209     C                             or cmzerr = '1'
077400050209     C                             or esito  = '1'
077500050209     C                   eval      wrkesito = '3'
077600050209     C                   endif
077700050209     C                   endif
077800050209     C*
077900050209     C                   ENDSR
078000050209     C***
078100010530
078200010530
078300010530
078400010530
078500010530      /TITLE Invio dei dati al punto operativo.
078600010530     C     opeini        BEGSR
078700010530     C*
078800010530     C* Inizializzo flag e contatori operativi
078900010530     C                   movel     '0'           flgGiro           1
079000010530     C                   movel     '0'           flgMulti          1
079100010530     C                   movel     '1'           flgStato          1
079200010615     C                   movel     '0'           flgOk             1
079300010530     C                   z-add     *zeros        cntNonEl         10 0
079400010530     C                   z-add     *zeros        depfil            3 0
079500010530     C                   z-add     *zeros        invfil            3 0
079600030407     C*
079700070214     C                   setoff                                       50
079800010530     C*
079900010530     C                   ENDSR
080000010530     C***
080100010530
080200010530
080300070411
080400070411     C     *pssr         BEGSR
080500070411     C*
080600070411     C                   if        %open(tivin00r)
080700070411     C                   close     tivin00r
080800070411     C                   endif
080900070411     C                   if        %open(fivabwwr)
081000070411     C                   close     fivabwwr
081100070411     C                   endif
081200070411     C                   if        %open(fivatwwr)
081300070411     C                   close     fivatwwr
081400070411     C                   endif
081500070411     C*
081600070411     C* Effettuo la chiamata al CLLE preposto
081700070411     C                   call(e)   'TITVVTC'
081800070411     C                   parm                    parccm
081900070411     C                   parm                    parmbr
082000070411     C                   parm      '2'           paropz
082100070411     C*
082200070411     C                   eval      wrkesito = '2'
082300070411     C*
082400070411     C                   seton                                        LR
082500070411     C*
082600070411     C                   ENDSR     '*CANCL'
082700070411     C***
082800070411
082900010530
083000990910
083100000613     C     *inzsr        BEGSR
083200990910     C*
083300990910     C     *entry        plist
083400990920     C                   parm                    tivlrds
083500990921     C                   parm      wrkesito      esito
083600000724     C                   parm                    prmlit
083700000710     C                   parm                    prmfir
083800010606     C*
083900010606     C* CALCOLA LA DATA CORRENTE
084000010606     C                   time                    wn14             14 0
084100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
084200010606     C                   z-add     wn8           g08dat
084300010606     C                   z-add     *zeros        g08inv
084400010606     C                   movel     '0'           g08err
084500010606     C                   call      'XSRDA8'
084600010606     C                   parm                    wlbda8
084700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
084800000613     C*
084900000613     C                   ENDSR
085000000613     C***
085100990908
085200070214     Otitvy0p   E            riepilogo         2
085300990915     O                                              'Upload via Internet'
085400990915     O                                           +1 'Traduzione TIVIN00R -'
085500020913     O                                           +1 'FIVABWWR'
085600990915     O                                           +1 'Testate bolle'
085700990915     O          E            riepilogo   2
085800990915     O                       wrkdata
085900990915     O                       wrkora              +1
086000990915     O                       procname            +1
086100990915     O          E            riepilogo   2
086200990915     O                                              'Cliente..................:'
086300990915     O                       VABCCM        z     +1
086400990915     O          E            riepilogo   2
086500990920     O                                              'Riferimento Strategi.....:'
086600990920     O                       vlrhdl              +1
086700990915     O          E            riepilogo   2
086800990915     O                                              'Giusti...................:'
086900971022     O                       §CTROK        2   +  1
087000990915     O          E            riepilogo   2
087100990915     O                                              'Sbagliati e corretti.....:'
087200971022     O                       §CTRMO        2   +  1
087300990915     O          E            riepilogo   2
087400990915     O                                              'Sbagliati e scartati.....:'
087500971022     O                       §CTRNO        2   +  1
087600000613
087700070214     Otitvy0ps  E            testdett          2
087800000613     O                                              'Upload via Internet'
087900000613     O                                           +1 'Traduzione TIVIN00R -'
088000020913     O                                           +1 'FIVABWWR'
088100000613     O                                           +1 'Testate bolle'
088200000616     O          E            testdett    3
088300000613     O                                           +2 'N° rec'
088400000613     O                                           +3 'Anteprima contenuto'
088500000616     O          E            rigadett    2
088600000613     O                       rrnum               +2
088700000621     O                       recko               +3
088800000616     O          E            findett     2
088900000613     O                       wrkdata
089000000613     O                       wrkora              +1
089100000613     O                       procname            +1
089200000616     O          E            findett     2
089300000613     O                                              'Cliente..................:'
089400000613     O                       VABCCM        z     +1
089500000616     O          E            findett     2
089600000613     O                                              'Riferimento Strategi.....:'
089700000613     O                       vlrhdl              +1
089800000616     O          E            findett     2
089900000613     O                                              'Giusti...................:'
090000000613     O                       §CTROK        2   +  1
090100000616     O          E            findett     2
090200000613     O                                              'Sbagliati e corretti.....:'
090300000613     O                       §CTRMO        2   +  1
090400000616     O          E            findett     2
090500000613     O                                              'Sbagliati e scartati.....:'
090600000613     O                       §CTRNO        2   +  1
090700000512** CMD - COMANDI CL
090800070214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
090900070214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
091000070214DLTOVR FILE(TITVY0P TITVY0PS) LVL(*)
091100000512
091200000512
