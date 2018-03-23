000100050518      /TITLE Upload via Internet: traduzione in FIVABWWR. (x PEG PEREGO cod.2022032-2022033-0592664)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700050518     Ftitvm8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050518     Ftitvm8ps  O    f  198        PRINTER usropn
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
002300050330     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100050518     D*------------------
004200050518     D* DS ARCHITETTURA
004300050518     D*------------------
004400050518     D kpjba         e ds                  inz
004500010330
004600010330
004700990915     C                   time                    wrkdata
004800990915     C                   time                    wrkora
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400010605     C                   exsr      rwvab
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
009800050518     C                   if        not %open(titvm8ps)
009900050518     C                   open      titvm8ps
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
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400990915     C*
011500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600000613     C*
011700050518     C                   if        %open(titvm8ps)
011800000616     C                   except    findett
011900050518     C                   close     titvm8ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200000613
013300000613     C*--------------------------------------------------------
013400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013500000613     C*--------------------------------------------------------
013600000621     C     STPR          BEGSR
013700000613     C*
013800050518     C                   if        not %open(titvm8p)
013900050518     C                   open      titvm8p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400050518     C                   if        %open(titvm8p)
014500050518     C                   close     titvm8p
014600990914     C                   endif
014700000613     C*
014800000613     C                   ENDSR
014900000613     C***
015000000613
015100010601
015200010601
015300010330     C*--------------------------------------------------------
015400050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015500010330     C*--------------------------------------------------------
015600010605     C     RWVAB         BEGSR
015700050408     C*
015800010330     C                   if        not %open(tivin00r)
015900010330     C                   open      tivin00r
016000010330     C                   endif
016100050408     C                   if        not %open(fivabwwr)
016200050408     C                   open      fivabwwr
016300010330     C                   endif
016400010330     C*
016500010604     C                   clear                   §CTROK
016600010604     C                   clear                   §CTRMO
016700010604     C                   clear                   §CTRNO
016800010330     C*
016900010330     C                   DO        *HIVAL
017000010330     C*
017100010330     C                   READ      tivin00r                               70
017200010618     C*
017300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017400010618     C                   if        vinflg = '1'
017500010618     C                   eval      flgOk = '1'
017600010618     C                   endif
017700010618     C*
017800010330     C                   if        vindta > *blanks
017900010330     C                   add       1             rrnum
018000010330     C*
018100010601     C                   if        *in70 = *off and
018200010330     C                             (vinflg = *blanks
018300010330     C                              or vinflg = '0'
018400010330     C                              or vinflg = '2')
018500010330     C*
018600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018700010711     C                   if        vinflg = *blanks or vinflg = '0'
018800010711     C                   clear                   vinmsg
018900010711     C                   endif
019000010601     C*
019100010330     C                   exsr      inzvar
019200010330     C                   exsr      defcam
019300010605     C                   exsr      impvab
019400010601     C*
019500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019600010605     C                   eval      depfil = VABLNP
019700010601     C                   exsr      repfil
019800010601     C                   if        depfil = invfil
019900021025     C                   if        vlrpoi = 999
020000021025     C                   MOVE(P)   invfil        VABFGS
020100021025     C                   else
020200021025     C                   MOVE(P)   vlrpoi        VABFGS
020300021025     C                   endif
020400010601     C*
020500010601     C                   exsr      PREELA
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900010604     C  N31              ADD       1             §CTROK            7 0
021000010604     C   32              ADD       1             §CTRMO            7 0
021100010604     C   31              ADD       1             §CTRNO            7 0
021200020205     C*
021300050408     C* Scarico il buffer del FIVAB
021400050408     C  N31              WRITE     FIVAB000
021500010604     C*
021600020722     C                   endif
021700020722     C*
021800010604     C                   if        *in31 = *off and
021900010604     C                             *in32 = *off
022000010604     C                   eval      vinflg = '1'
022100010604     C                   else
022200010604     C                   eval      recko = vindta
022300010604     C                   except    rigadett
022400010604     C                   eval      vinflg = '2'
022500010604     C                   endif
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400010330     C  N70              ENDdo
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300050408     C                   if        %open(fivabwwr)
026400050408     C                   close     fivabwwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200010330
028300010601
028400010601
028500010601
028600010601
028700010330     C*----------------------------------------------------*
028800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028900010330     C*----------------------------------------------------*
029000010330     C     INZVAR        BEGSR
029100010330     C*
029200020204     C                   Z-ADD     *zeros        Num5_0            5 0
029300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029400020322     C                   MOVEL     '0'           FlgCAS            1
029500010330     C*
029600010330     C                   ENDSR
029700010330     C*----------------------------------------------------*
029800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029900010330     C*----------------------------------------------------*
030000010330     C     DEFCAM        BEGSR
030100010330     C*
030200020204     C* Inizializzo il buffer del record da scrivere
030300050408     C                   CLEAR                   FIVAB000
030400020204     C* Imposto i valori di default...
030500050518     C                   EVAL      VABCCM = 2022032
030600050518     C                   EVAL      VABLNP = 202
030700050518     C                   EVAL      VABCTR = 000
030800050518     C                   EVAL      VABCBO = '1'
030900051021     C                   EVAL      VABCTM = '7A'
031000020204     C* ... e poi verifico se sono stati passati come parametri
031100020204     C                   IF        vlrppt > *blanks
031200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABCCM
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCTR
033100040301     C                   ENDIF
033200020204     C                   ENDIF
033300110802     C                   IF        %trim(%subst(vlrppt:14:2)) <> *blanks
033400110802     C                   EVAL      VABCTM=%trim(%subst(vlrppt:11:3))
033500110802     C                   ENDIF
033600020204     C                   ENDIF
033700020204     C*
033800010330     C                   ENDSR
033900010607     C*----------------------------------------------------*
034000050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034100010607     C*----------------------------------------------------*
034200010607     C     IMPVAB        BEGSR
034300010607     C*
034400010607     C                   SETOFF                                       3132
034500010607     C*
034600010607     C* Reperimento campi ALFA
034700010607     C*
034800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034900050518     C                   EVAL      VABRSD=%trim(%subst(vindta:19:35))
035000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035200020117     C* ==
035300050518     C                   EVAL      VABIND=%trim(%subst(vindta:54:35))
035400050518     C                   EVAL      VABLOD=%trim(%subst(vindta:99:35))
035500050518     C                   EVAL      VABPRD=%trim(%subst(vindta:133:3))
035600050518     C                   EVAL      VABRMA=%trim(%subst(vindta:1:10))
035700050901     C                   EVAL      VABNOT=%trim(%subst(vindta:186:35))
035800050901     C                   EVAL      VABNT2=%trim(%subst(vindta:186+35:35))
035900010607     C*
036000010607     C* Reperimento campi NUMERICI
036100020722     C                   MOVEL     DATCOR        VABAAS
036200020722     C                   MOVE      DATCOR        VABMGS
036300050518
036400051005     C* NSP/RMN
036500051005     C                   EVAL      PiStr=%trim(%subst(vindta:1:2)+
036600051005     C                                         %subst(vindta:6:5))
036700050408     C                   EXSR      CHKNUM
036800050408     C                   IF        PiInt=*on
036900051005     C                   Z-ADD     PiVal         VABNSP
037000050408     C                   Z-ADD     PiVal         VABRMN
037100050408     C                   ELSE
037200050518     C                   SETON                                        32
037300050408     C                   Z-ADD     *zeros        VABRMN
037400050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037500051005     C                             + ' ' + 'VABNSP VABRMN'
037600050408     C                   ENDIF
037700040420     C* CAD
037800050518     C                   EVAL      PiStr=%trim(%subst(vindta:89:10))
037900010607     C                   EXSR      CHKNUM
038000010607     C                   IF        PiInt=*on
038100020204     C                   Z-ADD     PiVal         Num5_0
038200020204     C                   MOVEL(p)  Num5_0        VABCAD
038300010607     C                   ELSE
038400010607     C                   SETON                                        32
038500020204     C                   EVAL      VABCAD = *zeros
038600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038700020204     C                             + ' ' + 'VABCAD'
038800010607     C                   ENDIF
038900040420     C* NCL
039000050518     C                   EVAL      PiStr=%trim(%subst(vindta:156:4))
039100010607     C                   EXSR      CHKNUM
039200010607     C                   IF        PiInt=*on
039300010607     C                   Z-ADD     PiVal         VABNCL
039400010607     C                   ELSE
039500010607     C                   SETON                                        32
039600010607     C                   Z-ADD     *zeros        VABNCL
039700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010607     C                             + ' ' + 'VABNCL'
039900010607     C                   ENDIF
040000040420     C* PKB
040100050518     C                   EVAL      PiStr=%trim(%subst(vindta:140:8))
040200010607     C                   EXSR      CHKNUM
040300010607     C                   IF        PiNum=*on
040400050330     C                   Z-ADD(H)  PiVal         VABPKB
040500010607     C                   ELSE
040600010607     C                   SETON                                        32
040700010607     C                   Z-ADD     *zeros        VABPKB
040800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040900010607     C                             + ' ' + 'VABPKB'
041000010607     C                   ENDIF
041100050518     C* VLB
041200050518     C                   EVAL      PiStr=%trim(%subst(vindta:148:8))
041300050518     C                   EXSR      CHKNUM
041400050518     C                   IF        PiNum=*on
041500050518     C                   Z-ADD(H)  PiVal         VABVLB
041600050518     C                   ELSE
041700050518     C                   SETON                                        32
041800050518     C                   Z-ADD     *zeros        VABPKB
041900050518     C                   EVAL      vinmsg = %trimr(vinmsg)
042000050518     C                             + ' ' + 'VABPKB'
042100050518     C                   ENDIF
042200050518      * Le spedizioni possono riguardare + codici
042300050518      * se codice 0592664 imposto ccm = 0592664 ctr = 000 tsp = C lnp = 059
042400060731     c*                  If        %subst(vlrksc:2:7) = '0592664'
042500060731     c*                  Eval      vabccm = 0592664
042600060731     c*                  Eval      vablnp = 059
042700060731     c*                  Eval      vabctr = 000
042800060731     c*                  Eval      vabtsp = 'C'
042900060731     c*                  Else
043000060731     C*                  If        %subst(vindta:160:2) = 'PI'
043100060731     c*                  Eval      vabccm = 2022032
043200060731     c*                  Eval      vablnp = 202
043300060731     c*                  Eval      vabctr = 000
043400060731     c*                  Eval      vabtsp = 'C'
043500060731     c*                  EndIf
043600060731     C*                  If        %subst(vindta:160:2) = 'GI'
043700060731     c*                  Eval      vabccm = 2022033
043800060731     c*                  Eval      vablnp = 202
043900060731     c*                  Eval      vabctr = 001
044000060731     c*                  Eval      vabtsp = 'C'
044100060731     c*                  EndIf
044200060731     c*                  EndIf
044300051013     C* CCM
044400051013     C                   EVAL      PiStr=%trim(%subst(vindta:256:7))
044500051013     C                   EXSR      CHKNUM
044600051013     C                   IF        PiInt=*on
044700051013     C                   Z-ADD     PiVal         VABCCM
044800051013     C                   ELSE
044900051013     C                   SETON                                        32
045000051013     C                   EVAL      vinmsg = %trimr(vinmsg)
045100051013     C                             + ' ' + 'VABCCM'
045200051013     C                   ENDIF
045300060904     C*
045400060904     C* Testo il codice cliente passato dal cliente x stabilire linea d partenza e codice tariffa
045500060904     C                   IF        VABCCM = 0592664
045600060904     C                   EVAL      VABLNP = 059
045700060904     C                   EVAL      VABCTR = 000
045800060904     C                   ENDIF
045900060904     C                   IF        VABCCM = 2022032
046000060904     C                   EVAL      VABLNP = 202
046100060904     C                   EVAL      VABCTR = 000
046200060904     C                   ENDIF
046300050901     C* DCR
046400050907     C                   IF        %subst(vindta:174:8) <> *blanks
046500050901     C                   EVAL      PiStr=%trim(%subst(vindta:174:8))
046600050901     C                   EXSR      CHKNUM
046700050901     C                   IF        PiInt=*on
046800050901     C                   Z-ADD     PiVal         VABDCR
046900050901     C* Inverto la data in quanto il cliente ce la passa nel formato: ggmmaaaa
047000050901     C                   z-add     VABDCR        g08dat
047100050901     C                   z-add     *zeros        g08inv
047200050901     C                   movel     '0'           g08err
047300050901     C                   call      'XSRDA8'
047400050901     C                   parm                    wlbda8
047500050901     C                   z-add     g08inv        VABDCR
047600050901     C*
047700050901     C                   ELSE
047800050901     C                   SETON                                        32
047900050901     C                   Z-ADD     *zeros        VABDCR
048000050901     C                   EVAL      vinmsg = %trimr(vinmsg)
048100050901     C                             + ' ' + 'VABDCR'
048200050901     C                   ENDIF
048300050907     C                   ENDIF
048400050901     C* HCR
048500050907     C                   IF        %subst(vindta:182:4) <> *blanks
048600050901     C                   EVAL      PiStr=%trim(%subst(vindta:182:4))
048700050901     C                   EXSR      CHKNUM
048800050901     C                   IF        PiInt=*on
048900050901     C                   Z-ADD     PiVal         VABHCR
049000050901     C                   ELSE
049100050901     C                   SETON                                        32
049200050901     C                   Z-ADD     *zeros        VABHCR
049300050901     C                   EVAL      vinmsg = %trimr(vinmsg)
049400050901     C                             + ' ' + 'VABHCR'
049500050901     C                   ENDIF
049600050907     C                   ENDIF
049700020322     C*
049800020322     C* Considerazioni finali su CBO/CAS
049900050908     C                   IF        FlgCAS = '1'
050000040713     C                   IF        VABCBO = '1'
050100020322     C                   EVAL      VABCBO = '4'
050200040713     C                   ENDIF
050300040713     C                   IF        VABCBO = '2'
050400040713     C                   EVAL      VABCBO = '6'
050500040713     C                   ENDIF
050600020322     C                   ENDIF
050700020204     C*
050800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050900020204     C                   EXSR      CHKIMPDIV
051000020204     C*
051100010607     C                   ENDSR
051200010607     C*----------------------------------------------------*
051300010601
051400020204
051500020204     C*----------------------------------------------------*
051600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051700020204     C*----------------------------------------------------*
051800020204     C     CHKIMPDIV     BEGSR
051900020204     C*
052000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052100020204     C                   Z-ADD     *zeros        wrkDec            9 9
052200020204     C*
052300020204     C* Come prima cosa effettuo considerazioni sulla divisa
052400020204     C                   IF        vabIAS > *zeros
052500020204     C                   IF        vabVAS <> 'EUR'
052600020204     C                   EVAL      vabVAS =  'ITL'
052700020204     C                   ENDIF
052800020204     C                   ENDIF
052900020204     C*
053000020204     C                   IF        vabCAS > *zeros
053100020204     C                   IF        vabVCA <> 'EUR'
053200020204     C                   EVAL      vabVCA =  'ITL'
053300020204     C                   ENDIF
053400020204     C                   ENDIF
053500020204     C*
053600020204     C                   IF        vabVMD > *zeros
053700020321     C                   IF        vabVAD <> 'EUR'
053800020204     C                   EVAL      vabVAD =  'ITL'
053900020204     C                   ENDIF
054000020204     C                   ENDIF
054100020204     C*
054200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054300020204     C                   Z-ADD     vabIAS        wrkDec
054400020204     C                   IF        wrkDec > *zeros
054500020204     C                   IF        vabVAS = 'ITL'
054600020204     C                   EVAL      vabIAS = *zeros
054700020204     C                   ENDIF
054800020204     C                   ENDIF
054900020204     C*
055000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
055100020204     C                   Z-ADD     vabCAS        wrkDec
055200020204     C                   IF        wrkDec > *zeros
055300020204     C                   IF        vabVCA = 'ITL'
055400020204     C                   EVAL      vabCAS = *zeros
055500020204     C                   ENDIF
055600020204     C                   ENDIF
055700020204     C*
055800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055900020204     C                   Z-ADD     vabVMD        wrkDec
056000020204     C                   IF        wrkDec > *zeros
056100020204     C                   IF        vabVAD = 'ITL'
056200020204     C                   EVAL      vabVMD = *zeros
056300020204     C                   ENDIF
056400020204     C                   ENDIF
056500020204     C*
056600020204     C                   ENDSR
056700020204     C***
056800020204
056900010330
057000010330
057100010330     C*----------------------------------------------------*
057200010330     C*  CONTROLLO NUMERICITA' CAMPI
057300010330     C*----------------------------------------------------*
057400010330     C     CHKNUM        BEGSR
057500010330     C*
057600010606     C                   IF        PiDecChr = *blanks
057700010606     C                   EVAL      PiDecChr = ','
057800010606     C                   ENDIF
057900010606     C*
058000010606     C                   CALL(e)   'ISNUMERIC'
058100010330     C                   PARM                    PiStr            30
058200010606     C                   PARM                    PiDecChr          1
058300010330     C                   PARM      *ZEROS        PiVal            30 9
058400010330     C                   PARM      '0'           PiInt             1
058500010330     C                   PARM      '0'           PiNum             1
058600010330     C                   IF        %error
058700010606     C                   EVAL      PiNum=*off
058800010330     C                   ENDIF
058900010330     C*
059000010330     C                   ENDSR
059100010330     C***
059200010330
059300010601
059400010601
059500010601
059600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059700010601     C     repfil        BEGSR
059800010601     C*
059900010601     C                   if        invfil = *zeros and
060000010601     C                             depfil > *zeros and
060100010629     C                             (vinflg = *blanks or
060200010629     C                              vinflg = *zeros)
060300010601     C
060400010601     C                   eval      invfil = depfil
060500010601     C                   endif
060600010601     C*
060700010601     C                   if        depfil <> invfil and
060800010601     C                             invfil > *zeros
060900010601     C                   eval      flgMulti = '1'
061000010601     C                   if        vinflg = *blanks
061100010601     C                   add       1             cntNonEl
061200010601     C                   endif
061300010601     C                   endif
061400010601     C*
061500010601     C                   if        vinflg = '2'
061600010601     C                   eval      flgStato = '2'
061700010601     C                   endif
061800010601     C*
061900010601     C                   ENDSR
062000010601     C***
062100010601
062200010601
062300010601
062400010330
062500010330
062600010330
062700990920      /TITLE Invio dei dati al punto operativo.
062800000613     C     invio         BEGSR
062900990920     C*
063000990920     C                   reset                   dscmz
063100010601     C                   move      invfil        cmzdst
063200990920     C                   eval      cmzfld = vlrfou
063300990920     C                   eval      cmzmbd = vlrhdl
063400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063500000710     C                   if        prmfir = *blanks
063600050408     C                   eval      cmzfla = 'FIVAB00F'
063700050408     C                   eval      cmzmba = 'FIVAB00F'
063800000710     C                   else
063900000710     C                   eval      cmzfla = prmfir
064000000710     C                   eval      cmzmba = prmfir
064100000710     C                   endif
064200990920     C                   eval      cmznrr = *zeros
064300990920     C                   move      §ctrok        cmznrr
064400021018     C                   eval      cmzlba = vlrfl1
064500990920     C                   call(e)   'TIS711C'
064600990920     C                   parm                    dscmz
064700990921     C                   parm      *blanks       esito
064800990920     C                   if        %error
064900990920     C                             or cmzerr = '1'
065000990921     C                             or esito  = '1'
065100000710     C                   eval      wrkesito = '3'
065200990920     C                   endif
065300990920     C*
065400000613     C                   ENDSR
065500990910
065600010601
065700010601
065800010601
065900010601
066000010601      /TITLE Invio dei dati al punto operativo.
066100010601     C     opeini        BEGSR
066200010601     C*
066300010601     C* Inizializzo flag e contatori operativi
066400010601     C                   movel     '0'           flgGiro           1
066500010601     C                   movel     '0'           flgMulti          1
066600010601     C                   movel     '1'           flgStato          1
066700010615     C                   movel     '0'           flgOk             1
066800010601     C                   z-add     *zeros        cntNonEl         10 0
066900010601     C                   z-add     *zeros        depfil            3 0
067000010601     C                   z-add     *zeros        invfil            3 0
067100010601     C*
067200010601     C                   ENDSR
067300010601     C***
067400010601
067500010601
067600010601
067700010330
067800010330
067900000613     C     *inzsr        BEGSR
068000990910     C*
068100990910     C     *entry        plist
068200990920     C                   parm                    tivlrds
068300990921     C                   parm      wrkesito      esito
068400000724     C                   parm                    prmlit
068500000710     C                   parm                    prmfir
068600010330     C*
068700010330     C* CALCOLA LA DATA CORRENTE
068800010330     C                   time                    wn14             14 0
068900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069000010330     C                   z-add     wn8           g08dat
069100010330     C                   z-add     *zeros        g08inv
069200010330     C                   movel     '0'           g08err
069300010330     C                   call      'XSRDA8'
069400010330     C                   parm                    wlbda8
069500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069600000613     C*
069700000613     C                   ENDSR
069800000613     C***
069900990908
070000050518     Otitvm8p   E            riepilogo         2
070100990915     O                                              'Upload via Internet'
070200990915     O                                           +1 'Traduzione TIVIN00R -'
070300050408     O                                           +1 'FIVABWWR'
070400990915     O                                           +1 'Testate bolle'
070500990915     O          E            riepilogo   2
070600990915     O                       wrkdata
070700990915     O                       wrkora              +1
070800990915     O                       procname            +1
070900990915     O          E            riepilogo   2
071000990915     O                                              'Cliente..................:'
071100010605     O                       VABCCM        z     +1
071200990915     O          E            riepilogo   2
071300990920     O                                              'Riferimento Strategi.....:'
071400990920     O                       vlrhdl              +1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Giusti...................:'
071700971022     O                       §CTROK        2   +  1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Sbagliati e corretti.....:'
072000971022     O                       §CTRMO        2   +  1
072100990915     O          E            riepilogo   2
072200990915     O                                              'Sbagliati e scartati.....:'
072300971022     O                       §CTRNO        2   +  1
072400000613
072500050518     Otitvm8ps  E            testdett          2
072600000613     O                                              'Upload via Internet'
072700000613     O                                           +1 'Traduzione TIVIN00R -'
072800050408     O                                           +1 'FIVABWWR'
072900000613     O                                           +1 'Testate bolle'
073000000616     O          E            testdett    3
073100000613     O                                           +2 'N° rec'
073200000613     O                                           +3 'Anteprima contenuto'
073300000616     O          E            rigadett    2
073400000613     O                       rrnum               +2
073500000621     O                       recko               +3
073600000616     O          E            findett     2
073700000613     O                       wrkdata
073800000613     O                       wrkora              +1
073900000613     O                       procname            +1
074000000616     O          E            findett     2
074100000613     O                                              'Cliente..................:'
074200010605     O                       VABCCM        z     +1
074300000616     O          E            findett     2
074400000613     O                                              'Riferimento Strategi.....:'
074500000613     O                       vlrhdl              +1
074600000616     O          E            findett     2
074700000613     O                                              'Giusti...................:'
074800000613     O                       §CTROK        2   +  1
074900000616     O          E            findett     2
075000000613     O                                              'Sbagliati e corretti.....:'
075100000613     O                       §CTRMO        2   +  1
075200000616     O          E            findett     2
075300000613     O                                              'Sbagliati e scartati.....:'
075400000613     O                       §CTRNO        2   +  1
075500000613** CMD - COMANDI CL
075600050518OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVM8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075700050518OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVM8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075800050518DLTOVR FILE(TITVM8P TITVM8PS) LVL(*)
075900010606
076000000613
