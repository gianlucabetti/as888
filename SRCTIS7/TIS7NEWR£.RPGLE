000100040226      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR. (x NEW IMS - Cod. 0060257)
000200990908     H dftactgrp(*yes)
000300040226     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700040226     FFIVATwwr  O    E             DISK    usropn
000800010605     Ftis7nip   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000010605     Ftis7nips  O    f  198        PRINTER usropn
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
003300040226     D parccm          s              8    INZ(*blanks)
003400040226     D parmbr          s             10    INZ(*blanks)
003500040226     D paropz          s              1    INZ(*blanks)
003600040226     D chkcall         s              1    INZ(*blanks)
003700010330     D*------------------
003800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003900010330     D*------------------
004000010330     D WLBDA8          DS                  INZ
004100010330     D  G08DAT                 1      8  0
004200010330     D  G08INV                 9     16  0
004300010330     D  G08ERR                17     17
004400010330     D  G08TGI                18     22  0
004500000313     D*
004600010330
004700010330
004800010330
004900990915     C                   time                    wrkdata
005000990915     C                   time                    wrkora
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400010601     C*
005500010601     C                   exsr      opeini
005600010605     C                   exsr      rwvab
005700040226     C*
005800040226     C* Effettuo la chiamata al CLLE preposto
005900050117     C                   call(e)   'TITVVTC'
006000040226     C                   parm                    parccm
006100040226     C                   parm                    parmbr
006200040226     C                   parm      '2'           paropz
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300010601     C     invfil        chain     azorg01l
008400000724     C                   if        %found
008500000724     C                   movel(p)  CMD(1)        depcmd
008600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000724     C*
008800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000724     C                   PARM                    depcmd
009200000724     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000613     C*
010600010605     C                   if        not %open(tis7nips)
010700010605     C                   open      tis7nips
010800000616     C                   except    testdett
010900000613     C                   endif
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
012600990915     C*
012700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012800000613     C*
012900010605     C                   if        %open(tis7nips)
013000000616     C                   except    findett
013100010605     C                   close     tis7nips
013200000613     C                   endif
013300000616     C*
013400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013500011113     C                   Z-ADD     150           LENGH            15 5
013600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013700000616     C                   PARM                    CMD(3)
013800000616     C                   PARM                    LENGH
013900000616     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601
014500010601
014600000613
014700000613     C*--------------------------------------------------------
014800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014900000613     C*--------------------------------------------------------
015000000621     C     STPR          BEGSR
015100000613     C*
015200010605     C                   if        not %open(tis7nip)
015300010605     C                   open      tis7nip
015400990915     C                   endif
015500990915     C*
015600990915     C                   except    riepilogo
015700990915     C*
015800010605     C                   if        %open(tis7nip)
015900010605     C                   close     tis7nip
016000990914     C                   endif
016100000613     C*
016200000613     C                   ENDSR
016300000613     C***
016400000613
016500010330
016600010601
016700010601
016800010601
016900010330     C*--------------------------------------------------------
017000040226     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF e FIVATWWR   *
017100010330     C*--------------------------------------------------------
017200010605     C     RWVAB         BEGSR
017300010330     C*
017400010330     C                   if        not %open(tivin00r)
017500010330     C                   open      tivin00r
017600010330     C                   endif
017700021113     C                   if        not %open(fivabwwr)
017800021113     C                   open      fivabwwr
017900010330     C                   endif
018000040226     C*
018100040226     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018200040226     C                   exsr      prevat
018300040226     C*
018400040226     C                   if        chkcall = '0'
018500040226     C*
018600040226     C                   if        not %open(fivatwwr)
018700040226     C                   open      fivatwwr
018800040226     C                   endif
018900010330     C*
019000040226     C                   clear                   §CTROKVB          7 0
019100040226     C                   clear                   §CTROKVT          7 0
019200040226     C                   clear                   §CTRMO            7 0
019300040226     C                   clear                   §CTRNO            7 0
019400010330     C*
019500010330     C                   DO        *HIVAL
019600010330     C*
019700010330     C                   READ      tivin00r                               70
019800010618     C*
019900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020000010618     C                   if        vinflg = '1'
020100010618     C                   eval      flgOk = '1'
020200010618     C                   endif
020300010618     C*
020400010330     C                   if        vindta > *blanks
020500010330     C                   add       1             rrnum
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700010330     C                   exsr      inzvar
021800010330     C                   exsr      defcam
021900010605     C                   exsr      impvab
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021113     C                   if        vlrpoi = 999
022600021113     C                   MOVE(P)   invfil        VABFGS
022700021113     C                   else
022800021113     C                   MOVE(P)   vlrpoi        VABFGS
022900021113     C                   endif
023000040226     C*
023100040226     C                   exsr      wrivat                                       => carico VAT
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700040226     C  N31              ADD       1             §CTROKVB
023800040226     C   32              ADD       1             §CTRMO
023900040226     C   31              ADD       1             §CTRNO
024000021113     C  N31              WRITE     FIVAB000
024100010604     C*
024200010604     C                   if        *in31 = *off and
024300010604     C                             *in32 = *off
024400010604     C                   eval      vinflg = '1'
024500010604     C                   else
024600010604     C                   eval      recko = vindta
024700010604     C                   except    rigadett
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000010330     C  N70              ENDdo
026100040226     C*
026200040226     C* Se chiamata al CL d preparazione file/membro x il VAT nn ok
026300040226     C                   else
026400040226     C                   eval      wrkesito = '2'
026500040226     C                   endif
026600010601     C
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800040226     C                   if        §ctrokvb > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500021113     C                   if        %open(fivabwwr)
029600021113     C                   close     fivabwwr
029700010330     C                   endif
029800040226     C                   if        %open(fivatwwr)
029900040226     C                   close     fivatwwr
030000040226     C                   endif
030100010601     C*
030200010601     C                   if        vlrpoi <> 999
030300010601     C                   eval      invfil = vlrpoi
030400010601     C                   endif
030500010330     C*
030600040226     C                   if        §ctrokvb > 0
030700010601     C                             and invfil > *zeros
030800010330     C                   exsr      invio
030900010330     C                   endif
031000010601     C*
031100010618     C                   if        flgGiro = '1'
031200010618     C                   exsr      endela
031300010618     C                   endif
031400010330     C*
031500010330     C                   ENDSR
031600010330     C***
031700010330
031800010601
031900010601
032000010601
032100010601
032200010330     C*----------------------------------------------------*
032300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032400010330     C*----------------------------------------------------*
032500010330     C     INZVAR        BEGSR
032600010330     C*
032700010330     C*
032800010330     C                   ENDSR
032900010330     C*----------------------------------------------------*
033000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
033100010330     C*----------------------------------------------------*
033200040226     C     DEFCAM        BEGSR
033300010330     C*
033400021113     C                   CLEAR                   FIVAB000
033500040226     C                   CLEAR                   FIVAT000
033600010605     C                   EVAL      VABCCM = 0060257
033700010605     C                   EVAL      VABLNP = 006
033800010605     C                   EVAL      VABCTR = 000
033900010606     C                   EVAL      VABCTM = '7Q'
034000010330     C*
034100010330     C                   ENDSR
034200010330     C*----------------------------------------------------*
034300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400010330     C*----------------------------------------------------*
034500010605     C     IMPVAB        BEGSR
034600010330     C*
034700010604     C                   SETOFF                                       3132
034800010605     C                   MOVEL     datcor        VABAAS
034900010607     C                   MOVE      datcor        VABMGS
035000010330     C*
035100010330     C* Reperimento campi ALFA
035200010605     C*
035300010605     C* Considerazioni sulla ragione sociale del destinatario da indicare
035400010605     C                   IF        %trim(%subst(vindta:343:35)) > *blanks
035500010605     C                   EVAL      VABRSD=%trim(%subst(vindta:343:35))
035600010606     C                   EVAL      VABRD2=%trim(%subst(vindta:17:40))
035700010605     C                   ELSE
035800010605     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
035900010606     C                   EVAL      VABRD2=%trim(%subst(vindta:17:40))
036000010605     C                   ENDIF
036100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036300020117     C* ==
036400010605     C*
036500010605     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
036600010605     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
036700010605     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
036800010605     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036900010605     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
037000010605     C*
037100010605     C* Considerazioni per il Fermo Deposito
037200010605     C                   IF        %trim(%subst(vindta:16:1)) = '1'
037300010605     C                   EVAL      VABFFD = 'S'
037400010605     C                   ENDIF
037500010605     C*
037600010612     C* Compongo i campi note
037700010605     C                   IF        %trim(%subst(vindta:197:40)) > *blanks
037800010605     C                   EVAL      VABNOT = %trim(%subst(vindta:197:40))
037900010605     C                   ENDIF
038000040226     C                   EVAL      VABNT2=%trim(%subst(vindta:237:40))
038100010605     C*
038200010605     C                   EVAL      VABRMA=%trim(%subst(vindta:308:10))
038300010330     C*
038400010605     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
038500010330     C                   EXSR      CHKNUM
038600010330     C                   IF        PiInt=*on
038700010605     C                   Z-ADD     PiVal         VABNCL
038800010330     C                   ELSE
038900010605     C                   SETON                                        32
039000010605     C                   Z-ADD     *zeros        VABNCL
039100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
039200010605     C                             + ' ' + 'VABNCL'
039300010330     C                   ENDIF
039400010605     C*
039500010605     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
039600010606     C                   EVAL      PiDecChr = '.'
039700010605     C                   EXSR      CHKNUM
039800010605     C                   IF        PiNum=*on
039900010606     C                   Z-ADD(H)  PiVal         VABPKB
040000010605     C                   ELSE
040100010605     C                   SETON                                        32
040200010605     C                   Z-ADD     *zeros        VABPKB
040300010605     C                   EVAL      vinmsg = %trimr(vinmsg)
040400010605     C                             + ' ' + 'VABPKB'
040500010605     C                   ENDIF
040600010605     C*
040700010605     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
040800010605     C                   EXSR      CHKNUM
040900010605     C                   IF        PiInt=*on
041000010606     C                   Z-ADD     PiVal         VABNSP
041100010606     C                   Z-ADD     PiVal         VABRMN
041200010605     C                   ELSE
041300010605     C                   SETON                                        32
041400010605     C                   Z-ADD     *zeros        VABNSP
041500010605     C                   Z-ADD     *zeros        VABRMN
041600010605     C                   EVAL      vinmsg = %trimr(vinmsg)
041700040226     C                             + ' ' + 'VABNSP VABRMN'
041800010605     C                   ENDIF
041900010605     C*
042000010605     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
042100010606     C                   EVAL      PiDecChr = ','
042200010605     C                   EXSR      CHKNUM
042300010605     C                   IF        PiNum=*on
042400010605     C                   Z-ADD     PiVal         VABCAS
042500010605     C                   ELSE
042600010605     C                   SETON                                        32
042700010605     C                   Z-ADD     *zeros        VABCAS
042800010605     C                   EVAL      vinmsg = %trimr(vinmsg)
042900010605     C                             + ' ' + 'VABCAS'
043000010605     C                   ENDIF
043100010605     C*
043200010605     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
043300010606     C                   EVAL      PiDecChr = ','
043400010605     C                   EXSR      CHKNUM
043500010605     C                   IF        PiNum=*on
043600010605     C                   Z-ADD     PiVal         VABIAS
043700010605     C                   ELSE
043800010605     C                   SETON                                        32
043900010605     C                   Z-ADD     *zeros        VABIAS
044000010605     C                   EVAL      vinmsg = %trimr(vinmsg)
044100010605     C                             + ' ' + 'VABIAS'
044200010605     C                   ENDIF
044300010330     C*
044400010605     C* Considerazioni finali sui valori sopra ricavati
044500010605     C                   IF        VABPKB = *zeros
044600010606     C                   EVAL      VABPKB = 1
044700010605     C                   ENDIF
044800010606     C                   IF        VABCAS >  *zeros
044900010612     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
045000010607     C                   EVAL      VABCBO = '4 '
045100020612     C                   IF        %subst(vindta:307:1)='N'
045200020612     C                   EVAL      VABTIC = *blanks
045300010605     C                   ELSE
045400010910     C                   EVAL      VABTIC = 'BC'
045500010605     C                   ENDIF
045600010607     C                   ELSE
045700010607     C                   EVAL      VABCBO = '1 '
045800010606     C                   ENDIF
045900010612     C                   IF        VABIAS >  *zeros
046000010612     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
046100010612     C                   ENDIF
046200010605     C*
046300010605     C* Applico eventuali forzature finali
046400010605     C                   IF        vlrppt > *blanks
046500010605     C                   EVAL      vlrppt = %trim(vlrppt)
046600010612     C                   MOVEL(P)  vlrppt        VABCCM
046700010605     C                   ENDIF
046800010605     C                   IF        VABCCM = 0501440
046900050316     C***                EVAL      VABPKB = 7
047000010605     C                   EVAL      VABLNP = 050
047100010606     C                   EVAL      VABCTM = *blanks
047200010605     C                   ENDIF
047300011113     C*
047400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047500011113     C                   EXSR      CHKIMPDIV
047600010330     C*
047700010330     C                   ENDSR
047800040226
047900040226
048000040226     C*----------------------------------------------------*
048100040226     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048200040226     C*----------------------------------------------------*
048300040226     C     WRIVAT        BEGSR
048400040226     C*
048500040226     C* Valorizzo l buffer di scrittura del FIVAT
048600040226     C                   eval      VATFGS = VABFGS
048700040226     C                   eval      VATAAS = VABAAS
048800040226     C                   eval      VATCCM = VABCCM
048900040226     C                   eval      VATLNP = VABLNP
049000040226     C                   eval      VATNSP = VABNSP
049100040226     C                   eval      VATTRC = 'B'
049200040226     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
049300040226     C                   write     FIVAT000
049400040226     C*
049500040226     C                   ENDSR
049600040226     C*----------------------------------------------------*
049700040226     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049800040226     C*----------------------------------------------------*
049900040226     C     PREVAT        BEGSR
050000040226     C*
050100040226     C* Compongo il nome del membro da dare al FIVATWWR
050200040226     C                   eval      parmbr = vlrhdl
050300040226     C                   movel     'M'           parmbr
050400040226     C                   eval      parccm = '0'+%editw(VABCCM:'       ')
050500040226     C                   eval      paropz = '1'
050600040226     C* Effettuo la chiamata al CLLE preposto
050700040226     C                   call(e)   'TITVVTC'
050800040226     C                   parm                    parccm
050900040226     C                   parm                    parmbr
051000040226     C                   parm                    paropz
051100040226     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051200040226     C                   if        %error
051300040226     C                   movel     '1'           chkcall
051400040226     C                   else
051500040226     C                   movel     '0'           chkcall
051600040226     C                   endif
051700040226     C*
051800040226     C                   ENDSR
051900990920
052000011113
052100010330
052200010330     C*----------------------------------------------------*
052300010330     C*  CONTROLLO NUMERICITA' CAMPI
052400010330     C*----------------------------------------------------*
052500010330     C     CHKNUM        BEGSR
052600010330     C*
052700010606     C                   IF        PiDecChr = *blanks
052800010606     C                   EVAL      PiDecChr = ','
052900010606     C                   ENDIF
053000010606     C*
053100010606     C                   CALL(e)   'ISNUMERIC'
053200010330     C                   PARM                    PiStr            30
053300010606     C                   PARM                    PiDecChr          1
053400010330     C                   PARM      *ZEROS        PiVal            30 9
053500010330     C                   PARM      '0'           PiInt             1
053600010330     C                   PARM      '0'           PiNum             1
053700010330     C                   IF        %error
053800010606     C                   EVAL      PiNum=*off
053900010330     C                   ENDIF
054000010330     C*
054100010330     C                   ENDSR
054200010330     C***
054300010330
054400011113
054500011113
054600011113     C*----------------------------------------------------*
054700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054800011113     C*----------------------------------------------------*
054900011113     C     CHKIMPDIV     BEGSR
055000011113     C*
055100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055200011113     C                   Z-ADD     *zeros        wrkDec            9 9
055300011113     C*
055400011113     C* Come prima cosa effettuo considerazioni sulla divisa
055500011113     C                   IF        vabIAS > *zeros
055600011113     C                   IF        vabVAS <> 'EUR'
055700011113     C                   EVAL      vabVAS =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabCAS > *zeros
056200011113     C                   IF        vabVCA <> 'EUR'
056300011113     C                   EVAL      vabVCA =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   IF        vabVMD > *zeros
056800011113     C                   IF        vabVAD <> 'EUR'
056900011113     C                   EVAL      vabVAD =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057400011113     C                   Z-ADD     vabIAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVAS = 'ITL'
057700011113     C                   EVAL      vabIAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058200011113     C                   Z-ADD     vabCAS        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVCA = 'ITL'
058500011113     C                   EVAL      vabCAS = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059000011113     C                   Z-ADD     vabVMD        wrkDec
059100011113     C                   IF        wrkDec > *zeros
059200011113     C                   IF        vabVAD = 'ITL'
059300011113     C                   EVAL      vabVMD = *zeros
059400011113     C                   ENDIF
059500011113     C                   ENDIF
059600011113     C*
059700011113     C                   ENDSR
059800011113     C***
059900011113
060000011113
060100011113
060200010601
060300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
060400010601     C     repfil        BEGSR
060500010601     C*
060600010601     C                   if        invfil = *zeros and
060700010601     C                             depfil > *zeros and
060800010628     C                             (vinflg = *blanks or
060900010628     C                              vinflg = *zeros)
061000010601     C
061100010601     C                   eval      invfil = depfil
061200010601     C                   endif
061300010601     C*
061400010601     C                   if        depfil <> invfil and
061500010601     C                             invfil > *zeros
061600010601     C                   eval      flgMulti = '1'
061700010601     C                   if        vinflg = *blanks
061800010601     C                   add       1             cntNonEl
061900010601     C                   endif
062000010601     C                   endif
062100010601     C*
062200010601     C                   if        vinflg = '2'
062300010601     C                   eval      flgStato = '2'
062400010601     C                   endif
062500010601     C*
062600010601     C                   ENDSR
062700010601     C***
062800010601
062900010601
063000010601
063100010330
063200010330
063300010330
063400990920      /TITLE Invio dei dati al punto operativo.
063500000613     C     invio         BEGSR
063600040226     C*
063700040226     C* 1° invio FIVAT
063800040226     C                   reset                   dscmz
063900040226     C                   move      vlrpoi        cmzdst
064000040226     C                   eval      cmzfld = 'FIVATWWR'
064100040226     C                   eval      cmzmbd = vlrhdl
064200040226     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300040226     C                   eval      cmzfla = 'FIVAT00F'
064400040226     C                   eval      cmzmba = 'FIVAT00F'
064500040226     C                   eval      cmznrr = *zeros
064600040226     C                   move      §ctrokvt      cmznrr
064700040226     C                   eval      cmzlba = vlrfl1
064800040226     C                   call(e)   'TIS711C'
064900040226     C                   parm                    dscmz
065000040226     C                   parm      *blanks       esito
065100040226     C                   if        %error
065200040226     C                             or cmzerr = '1'
065300040226     C                             or esito  = '1'
065400040226     C                   eval      wrkesito = '3'
065500040226     C                   else
065600040226     C*
065700040226     C* 2° invio FIVAB
065800990920     C*
065900990920     C                   reset                   dscmz
066000010601     C                   move      invfil        cmzdst
066100990920     C                   eval      cmzfld = vlrfou
066200990920     C                   eval      cmzmbd = vlrhdl
066300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400021113     C                   eval      cmzfla = 'FIVAB00F'
066500021113     C                   eval      cmzmba = 'FIVAB00F'
066600990920     C                   eval      cmznrr = *zeros
066700040226     C                   move      §ctrokvb      cmznrr
066800021025     C                   eval      cmzlba = vlrfl1
066900990920     C                   call(e)   'TIS711C'
067000990920     C                   parm                    dscmz
067100990921     C                   parm      *blanks       esito
067200990920     C                   if        %error
067300990920     C                             or cmzerr = '1'
067400990921     C                             or esito  = '1'
067500000710     C                   eval      wrkesito = '3'
067600990920     C                   endif
067700040226     C                   endif
067800990920     C*
067900000613     C                   ENDSR
068000990910
068100010601
068200010601
068300010601
068400010601
068500010601      /TITLE Invio dei dati al punto operativo.
068600010601     C     opeini        BEGSR
068700010601     C*
068800010601     C* Inizializzo flag e contatori operativi
068900010601     C                   movel     '0'           flgGiro           1
069000010601     C                   movel     '0'           flgMulti          1
069100010601     C                   movel     '1'           flgStato          1
069200010615     C                   movel     '0'           flgOk             1
069300010601     C                   z-add     *zeros        cntNonEl         10 0
069400010601     C                   z-add     *zeros        depfil            3 0
069500010601     C                   z-add     *zeros        invfil            3 0
069600010601     C*
069700010601     C                   ENDSR
069800010601     C***
069900010601
070000010601
070100010601
070200010330
070300010330
070400000613     C     *inzsr        BEGSR
070500990910     C*
070600990910     C     *entry        plist
070700990920     C                   parm                    tivlrds
070800990921     C                   parm      wrkesito      esito
070900000724     C                   parm                    prmlit
071000000710     C                   parm                    prmfir
071100010330     C*
071200010330     C* CALCOLA LA DATA CORRENTE
071300010330     C                   time                    wn14             14 0
071400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071500010330     C                   z-add     wn8           g08dat
071600010330     C                   z-add     *zeros        g08inv
071700010330     C                   movel     '0'           g08err
071800010330     C                   call      'XSRDA8'
071900010330     C                   parm                    wlbda8
072000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072100000613     C*
072200000613     C                   ENDSR
072300000613     C***
072400990908
072500010605     Otis7nip   E            riepilogo         2
072600990915     O                                              'Upload via Internet'
072700990915     O                                           +1 'Traduzione TIVIN00R -'
072800040226     O                                           +1 'FIVABWWR/FIVATWWR'
072900990915     O                                           +1 'Testate bolle'
073000990915     O          E            riepilogo   2
073100990915     O                       wrkdata
073200990915     O                       wrkora              +1
073300990915     O                       procname            +1
073400990915     O          E            riepilogo   2
073500990915     O                                              'Cliente..................:'
073600010605     O                       VABCCM        z     +1
073700990915     O          E            riepilogo   2
073800990920     O                                              'Riferimento Strategi.....:'
073900990920     O                       vlrhdl              +1
074000990915     O          E            riepilogo   2
074100990915     O                                              'Giusti...................:'
074200040226     O                       §CTROKVB      2   +  1
074300990915     O          E            riepilogo   2
074400990915     O                                              'Sbagliati e corretti.....:'
074500971022     O                       §CTRMO        2   +  1
074600990915     O          E            riepilogo   2
074700990915     O                                              'Sbagliati e scartati.....:'
074800971022     O                       §CTRNO        2   +  1
074900000613
075000010605     Otis7nips  E            testdett          2
075100000613     O                                              'Upload via Internet'
075200000613     O                                           +1 'Traduzione TIVIN00R -'
075300040226     O                                           +1 'FIVABWWR/FIVATWWR'
075400000613     O                                           +1 'Testate bolle'
075500000616     O          E            testdett    3
075600000613     O                                           +2 'N° rec'
075700000613     O                                           +3 'Anteprima contenuto'
075800000616     O          E            rigadett    2
075900000613     O                       rrnum               +2
076000000621     O                       recko               +3
076100000616     O          E            findett     2
076200000613     O                       wrkdata
076300000613     O                       wrkora              +1
076400000613     O                       procname            +1
076500000616     O          E            findett     2
076600000613     O                                              'Cliente..................:'
076700010605     O                       VABCCM        z     +1
076800000616     O          E            findett     2
076900000613     O                                              'Riferimento Strategi.....:'
077000000613     O                       vlrhdl              +1
077100000616     O          E            findett     2
077200000613     O                                              'Giusti...................:'
077300040226     O                       §CTROKVB      2   +  1
077400000616     O          E            findett     2
077500000613     O                                              'Sbagliati e corretti.....:'
077600000613     O                       §CTRMO        2   +  1
077700000616     O          E            findett     2
077800000613     O                                              'Sbagliati e scartati.....:'
077900000613     O                       §CTRNO        2   +  1
078000000613** CMD - COMANDI CL
078100011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7NIP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
078200011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7NIPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
078300010605DLTOVR FILE(TIS7NIP TIS7NIPS) LVL(*)
078400010606
078500000613
