000100041019      /TITLE Upload via Internet: traduzione in FIVATWWR. (x RICOH Cod. 0431960)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600041019     FFIVATWWR  O    E             DISK    usropn
000700041018     Ftitvf3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900041018     Ftitvf3ps  O    f  198        PRINTER usropn
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
003200060911     D depDest         s              3    INZ(*blanks)
003300060911     D curDest         s              3    INZ(*blanks)
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
009700041018     C                   if        not %open(titvf3ps)
009800041018     C                   open      titvf3ps
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
012000041018     C                   if        %open(titvf3ps)
012100000616     C                   except    findett
012200041018     C                   close     titvf3ps
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
014300041018     C                   if        not %open(titvf3p)
014400041018     C                   open      titvf3p
014500990915     C                   endif
014600990915     C*
014700990915     C                   except    riepilogo
014800990915     C*
014900041018     C                   if        %open(titvf3p)
015000041018     C                   close     titvf3p
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
016100041019     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVAT00F              *
016200010330     C*--------------------------------------------------------
016300020321     C     RWVAT         BEGSR
016400010330     C*
016500010330     C                   if        not %open(tivin00r)
016600010330     C                   open      tivin00r
016700010330     C                   endif
016800041019     C                   if        not %open(fivatwwr)
016900041019     C                   open      fivatwwr
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
019700060911     C*
019800060911     C* Verifico la rottura d codice x destinatario (è pilotata dal cliente ed è 1 zozzeria
019900060911     C* ma nn c'è stato verso d trovare 1 punto d'incontro con il cliente, x cui....)
020000060911     C                   eval      curDest = %subst(vindta:42:3)
020100060911     C                   if        curDest = *all'0' OR curDest <> depDest
020200060911     C                   exsr      inzvar
020300060911     C                   exsr      defcam
020400060911     C*
020500060911     C* Salvo la rottura d codice del destinatario corrente
020600060911     C                   eval      depDest = curDest
020700060911     C*
020800060911     C* X stesso destinatario invece....
020900060911     C                   else
021000060911     C                   seton                                        55
021100060911     C                   endif
021200060911     C*
021300060911     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400060911     C                   eval      depfil = VATLNP
021500060911     C                   exsr      repfil
021600060911     C                   if        depfil = invfil
021700060911     C                   if        vlrpoi = 999
021800060911     C                   MOVE(P)   invfil        VATFGS
021900060911     C                   else
022000060911     C                   MOVE(P)   vlrpoi        VATFGS
022100060911     C                   endif
022200060911     C*
022300060911     C                   exsr      PREELA
022400060911     C*
022500060911     C* A questo punto valorizzo il buffer d output
022600060911     C                   exsr      impvat
022700060911     C*
022800060911     C* Scrivo sempre il buffer d output
022900060911     C                   exsr      wrifiles
023000010604     C*
023100010604     C                   if        *in31 = *off and
023200010604     C                             *in32 = *off
023300010604     C                   eval      vinflg = '1'
023400010604     C                   else
023500010604     C                   eval      recko = vindta
023600010604     C                   except    rigadett
023700010604     C                   eval      vinflg = '2'
023800010604     C                   endif
023900010604     C                   endif
024000010601     C*
024100010604     C                   endif
024200010604     C*
024300010330     C                   else
024400010330     C                   eval      vinflg = '1'
024500010330     C                   endif
024600010601     C*
024700010601     C  N70              update    tivin000
024800010330     C*
024900010330     C  N70              ENDdo
025000010601     C*
025100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025200010601     C                   if        cntNonEl = *zeros or
025300010601     C                             flgMulti = '0'
025400010330     C* Se non ci sono record con errori ...
025500010601     C                   if        §ctrno = 0 and
025600010604     C                             §ctrmo = 0 and
025700010601     C                             flgStato <> '2'
025800010330     C* ... restituisco esito OK.
025900010330     C                   eval      wrkesito = '0'
026000010330     C                   else
026100010330     C                   if        §ctrok > 0
026200010330     C                   eval      wrkesito = '1'
026300010330     C                   else
026400010615     C                   if        flgOk = '0'
026500010615     C                   eval      wrkesito = '2'
026600010615     C                   else
026700010615     C                   eval      wrkesito = '6'
026800010615     C                   endif
026900010330     C                   endif
027000010330     C                   endif
027100010601     C                   else
027200010601     C                   eval      wrkesito = '9'
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        %open(tivin00r)
027600010330     C                   close     tivin00r
027700010330     C                   endif
027800041019     C                   if        %open(fivatwwr)
027900041019     C                   close     fivatwwr
028000010330     C                   endif
028100010601     C*
028200010601     C                   if        vlrpoi <> 999
028300010601     C                   eval      invfil = vlrpoi
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        §ctrok > 0
028700010601     C                             and invfil > *zeros
028800010330     C                   exsr      invio
028900010330     C                   endif
029000010601     C*
029100010618     C                   if        flgGiro = '1'
029200010601     C                   exsr      endela
029300010618     C                   endif
029400010330     C*
029500010330     C                   ENDSR
029600010330     C***
029700010330
029800010601
029900010601
030000010601
030100010601
030200060911     C*----------------------------------------------------*
030300060911     C*  SCRITTURA BUFFER FILE OUTPUT
030400060911     C*----------------------------------------------------*
030500060911     C     WRIFILES      BEGSR
030600060911     C*
030700060911     C* Ebbene...
030800060911     C*
030900060911     C  N31              ADD       1             §CTROK            7 0
031000060911     C   32              ADD       1             §CTRMO            7 0
031100060911     C   31              ADD       1             §CTRNO            7 0
031200060911     C*
031300060911     C  N31              WRITE     FIVAT000
031400060911     C*
031500060911     C* Dopo lo scarico del buffer spengo l'indicatore che guida l'importazione dei campi chiave
031600060911     C                   SETOFF                                       55
031700060911     C*
031800060911     C                   ENDSR
031900010330     C*----------------------------------------------------*
032000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032100010330     C*----------------------------------------------------*
032200040607     C     INZVAR        BEGSR
032300010330     C*
032400010330     C*
032500010330     C                   ENDSR
032600010330     C*----------------------------------------------------*
032700041018     C*  IMPOSTAZIONE CAMPI COSTANTI
032800010330     C*----------------------------------------------------*
032900010330     C     DEFCAM        BEGSR
033000010330     C*
033100020204     C* Inizializzo il buffer del record da scrivere
033200041019     C                   CLEAR                   FIVAT000
033300020204     C* Imposto i valori di default...
033400041018     C                   EVAL      VATCCM = 0431960
033500041018     C                   EVAL      VATLNP = 043
033600020321     C                   EVAL      VATTRC = 'E'
033700020204     C* ... e poi verifico se sono stati passati come parametri
033800020204     C                   IF        vlrppt > *blanks
033900040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
034000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034100020204     C                   EXSR      CHKNUM
034200020204     C                   IF        PiInt=*on
034300020321     C                   Z-ADD     PiVal         VATCCM
034400020204     C                   ENDIF
034500040607     C                   ENDIF
034600040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
034700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034800020204     C                   EXSR      CHKNUM
034900020204     C                   IF        PiInt=*on
035000020321     C                   Z-ADD     PiVal         VATLNP
035100020204     C                   ENDIF
035200040607     C                   ENDIF
035300020321     C                   ENDIF
035400020204     C*
035500010330     C                   ENDSR
035600010607     C*----------------------------------------------------*
035700041019     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
035800010607     C*----------------------------------------------------*
035900020321     C     IMPVAT        BEGSR
036000010607     C*
036100010607     C                   SETOFF                                       3132
036200010607     C*
036300010607     C* Reperimento campi ALFA
036400080527     C                   EVAL      VATNOT=%subst(vindta:45:15)
036500020329     C*
036600020329     C                   MOVEL     DATCOR        VATAAS
036700010607     C*
036800010607     C* Reperimento campi NUMERICI
036900060911     C                   IF        *IN55 = *OFF
037000041019     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
037100020304     C                   EXSR      CHKNUM
037200020304     C                   IF        PiInt=*on
037300020321     C                   Z-ADD     PiVal         VATNSP
037400020304     C                   ELSE
037500041018     C                   SETON                                        31
037600020321     C                   Z-ADD     *zeros        VATNSP
037700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037800020321     C                             + ' ' + 'VATNSP'
037900020304     C                   ENDIF
038000060911     C                   ENDIF
038100020204     C*
038200010607     C                   ENDSR
038300010607     C*----------------------------------------------------*
038400010601
038500010330
038600010330     C*----------------------------------------------------*
038700010330     C*  CONTROLLO NUMERICITA' CAMPI
038800010330     C*----------------------------------------------------*
038900010330     C     CHKNUM        BEGSR
039000010330     C*
039100010606     C                   IF        PiDecChr = *blanks
039200010606     C                   EVAL      PiDecChr = ','
039300010606     C                   ENDIF
039400010606     C*
039500010606     C                   CALL(e)   'ISNUMERIC'
039600010330     C                   PARM                    PiStr            30
039700010606     C                   PARM                    PiDecChr          1
039800010330     C                   PARM      *ZEROS        PiVal            30 9
039900010330     C                   PARM      '0'           PiInt             1
040000010330     C                   PARM      '0'           PiNum             1
040100010330     C                   IF        %error
040200010606     C                   EVAL      PiNum=*off
040300010330     C                   ENDIF
040400010330     C*
040500010330     C                   ENDSR
040600010330     C***
040700010330
040800010601
040900010601
041000010601
041100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041200010601     C     repfil        BEGSR
041300010601     C*
041400010601     C                   if        invfil = *zeros and
041500010601     C                             depfil > *zeros and
041600010629     C                             (vinflg = *blanks or
041700010629     C                              vinflg = *zeros)
041800010601     C
041900010601     C                   eval      invfil = depfil
042000010601     C                   endif
042100010601     C*
042200010601     C                   if        depfil <> invfil and
042300010601     C                             invfil > *zeros
042400010601     C                   eval      flgMulti = '1'
042500010601     C                   if        vinflg = *blanks
042600010601     C                   add       1             cntNonEl
042700010601     C                   endif
042800010601     C                   endif
042900010601     C*
043000010601     C                   if        vinflg = '2'
043100010601     C                   eval      flgStato = '2'
043200010601     C                   endif
043300010601     C*
043400010601     C                   ENDSR
043500010601     C***
043600010601
043700010601
043800010601
043900010330
044000010330
044100010330
044200990920      /TITLE Invio dei dati al punto operativo.
044300000613     C     invio         BEGSR
044400990920     C*
044500990920     C                   reset                   dscmz
044600010601     C                   move      invfil        cmzdst
044700990920     C                   eval      cmzfld = vlrfou
044800990920     C                   eval      cmzmbd = vlrhdl
044900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045000000710     C                   if        prmfir = *blanks
045100041019     C                   eval      cmzfla = 'FIVAT00F'
045200041019     C                   eval      cmzmba = 'FIVAT00F'
045300000710     C                   else
045400000710     C                   eval      cmzfla = prmfir
045500000710     C                   eval      cmzmba = prmfir
045600000710     C                   endif
045700990920     C                   eval      cmznrr = *zeros
045800990920     C                   move      §ctrok        cmznrr
045900021018     C                   eval      cmzlba = vlrfl1
046000990920     C                   call(e)   'TIS711C'
046100990920     C                   parm                    dscmz
046200990921     C                   parm      *blanks       esito
046300990920     C                   if        %error
046400990920     C                             or cmzerr = '1'
046500990921     C                             or esito  = '1'
046600000710     C                   eval      wrkesito = '3'
046700990920     C                   endif
046800990920     C*
046900000613     C                   ENDSR
047000990910
047100010601
047200010601
047300010601
047400010601
047500010601      /TITLE Invio dei dati al punto operativo.
047600010601     C     opeini        BEGSR
047700010601     C*
047800010601     C* Inizializzo flag e contatori operativi
047900010601     C                   movel     '0'           flgGiro           1
048000010601     C                   movel     '0'           flgMulti          1
048100010601     C                   movel     '1'           flgStato          1
048200010615     C                   movel     '0'           flgOk             1
048300010601     C                   z-add     *zeros        cntNonEl         10 0
048400010601     C                   z-add     *zeros        depfil            3 0
048500010601     C                   z-add     *zeros        invfil            3 0
048600010601     C*
048700010601     C                   ENDSR
048800010601     C***
048900010601
049000010601
049100010601
049200010330
049300010330
049400000613     C     *inzsr        BEGSR
049500990910     C*
049600990910     C     *entry        plist
049700990920     C                   parm                    tivlrds
049800990921     C                   parm      wrkesito      esito
049900000724     C                   parm                    prmlit
050000000710     C                   parm                    prmfir
050100010330     C*
050200010330     C* CALCOLA LA DATA CORRENTE
050300010330     C                   time                    wn14             14 0
050400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050500010330     C                   z-add     wn8           g08dat
050600010330     C                   z-add     *zeros        g08inv
050700010330     C                   movel     '0'           g08err
050800010330     C                   call      'XSRDA8'
050900010330     C                   parm                    wlbda8
051000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051100000613     C*
051200000613     C                   ENDSR
051300000613     C***
051400990908
051500041018     Otitvf3p   E            riepilogo         2
051600990915     O                                              'Upload via Internet'
051700990915     O                                           +1 'Traduzione TIVIN00R -'
051800041019     O                                           +1 'FIVATWWR'
051900990915     O                                           +1 'Testate bolle'
052000990915     O          E            riepilogo   2
052100990915     O                       wrkdata
052200990915     O                       wrkora              +1
052300990915     O                       procname            +1
052400990915     O          E            riepilogo   2
052500990915     O                                              'Cliente..................:'
052600020321     O                       VATCCM        z     +1
052700990915     O          E            riepilogo   2
052800990920     O                                              'Riferimento Strategi.....:'
052900990920     O                       vlrhdl              +1
053000990915     O          E            riepilogo   2
053100990915     O                                              'Giusti...................:'
053200971022     O                       §CTROK        2   +  1
053300990915     O          E            riepilogo   2
053400990915     O                                              'Sbagliati e corretti.....:'
053500971022     O                       §CTRMO        2   +  1
053600990915     O          E            riepilogo   2
053700990915     O                                              'Sbagliati e scartati.....:'
053800971022     O                       §CTRNO        2   +  1
053900000613
054000041018     Otitvf3ps  E            testdett          2
054100000613     O                                              'Upload via Internet'
054200000613     O                                           +1 'Traduzione TIVIN00R -'
054300041019     O                                           +1 'FIVATWWR'
054400000613     O                                           +1 'Testate bolle'
054500000616     O          E            testdett    3
054600000613     O                                           +2 'N° rec'
054700000613     O                                           +3 'Anteprima contenuto'
054800000616     O          E            rigadett    2
054900000613     O                       rrnum               +2
055000000621     O                       recko               +3
055100000616     O          E            findett     2
055200000613     O                       wrkdata
055300000613     O                       wrkora              +1
055400000613     O                       procname            +1
055500000616     O          E            findett     2
055600000613     O                                              'Cliente..................:'
055700020321     O                       VATCCM        z     +1
055800000616     O          E            findett     2
055900000613     O                                              'Riferimento Strategi.....:'
056000000613     O                       vlrhdl              +1
056100000616     O          E            findett     2
056200000613     O                                              'Giusti...................:'
056300000613     O                       §CTROK        2   +  1
056400000616     O          E            findett     2
056500000613     O                                              'Sbagliati e corretti.....:'
056600000613     O                       §CTRMO        2   +  1
056700000616     O          E            findett     2
056800000613     O                                              'Sbagliati e scartati.....:'
056900000613     O                       §CTRNO        2   +  1
057000000613** CMD - COMANDI CL
057100041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
057200041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
057300041018DLTOVR FILE(TITVF3P TITVF3PS) LVL(*)
057400010606
057500000613
