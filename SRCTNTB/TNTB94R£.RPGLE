000100020208     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020208      *--------------------------------------------------------------------------------------------*
000300120418      *     Gestine tabella CUR per aggiornare i profili con la GAICUR richiesta                   *
000400020208      *--------------------------------------------------------------------------------------------*
000500020208     FAzOrg01l  IF   E           K DISK
000600070126     FTntbe01l  uF A E           K Disk
000700120418     FTnTb94d   CF   E             WORKSTN
000800020208      *--------------------------------------------------------------------------------------------*
000900020208      * Data structure                                                                             *
001000020208      *--------------------------------------------------------------------------------------------*
001100020208      *---
001200020208      * Schiere memorizzazione dati correnti
001300020208      *---
001400020320     D msg             S             78    DIM(6) CTDATA PERRCD(1)              *Messaggi video
001500120418     D CMDchk          S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
001600070126     d
001700120418     D Dcur          E DS                                                       *ds di procedura
001800020208      *
001900020208     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000120418     D fnlv55ds      E DS
002100120419     D tibs02ds      E DS
002200020208     D kpjba         E DS                                                       *architettura
002300120420     D dsemail       E DS
002400120418     D***
002500120418     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002600120418     D***
002700120418     D WLBDAT          DS                  INZ
002800120418     D  G02DAT                 1      8  0
002900120418     D  G02INV                 9     16  0
003000120418     D  G02ERR                17     17
003100120418     D  G02TGI                18     22  0
003200120420     d
003300120420     D tibs39ds        DS                  INZ
003400120420     D  bs39agg                1      1
003500120420     D  bs39sim                2      2
003600120420     D  bs39email              3     27
003700120420     d
003800120418     d Dataiso         s               d   datfmt(*iso)
003900120418     d Dataeur         s               d   datfmt(*eur)
004000020208      *--------------------------------------------------------------------------------------------*
004100020208      * Main lines
004200020208      *--------------------------------------------------------------------------------------------*
004300020208      *
004400020208      * Operazioni iniziali
004500020208      *
004600020208     C                   EXSR      RutInz
004700020208      *
004800020208      * Gestione video
004900020208      *
005000020208     C     $Fine         DOUEQ     'S'
005100070126     c     TipVid        CASEQ     '1'           GesVd1                          *Sfile
005200020208     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile
005300020208     C                   ENDCS
005400020208     C                   ENDDO
005500020208      *
005600020208     C                   SETON                                        LR
005700020208      *--------------------------------------------------------------------------------------------*
005800020208      * GesVd1 - Gestione videata immissione nuova tabella -
005900020208      *--------------------------------------------------------------------------------------------*
006000020208     C     GesVd1        BEGSR
006100020208      *
006200020208      * Inizializzazione videata
006300120420     c                   clear                   v1cemail
006400120420     c                   clear                   v1clib
006500120419     c                   eval      v1clib='?'
006600020208      *
006700070126     c     for01         tag
006800120418     C                   EXFMT     Tb94v1
006900070126     c                   clear                   v1cmsg
007000120420     c
007100070126     c                   setoff                                       2890
007200120420     c                   setoff                                       4041
007300020208      *
007400070126     c* F3 -  fine
007500070126     c                   if        *inkc
007600070126     C                   EXSR      F03Ges
007700070126     c                   goto      endv1
007800070126     c                   endif
007900120420
008000120420     c* se premuti F13 o F10 - controllo indirizzo e-mail
008100120420     c                   if        *inkm or *inkj
008200120420     c                   exsr      CtrEmail
008300120420     c
008400120420     c                   if        *in90
008500120420     c                   goto      for01
008600120420     c                   endif
008700120420     c                   endif
008800120420     c
008900120420     c* F10 -  Simulazione aggiornamenti
009000120420     c                   if        *inkj
009100120420     c                   clear                   kpjbu
009200120420     c                   clear                   tibs39ds
009300120420     c* solo simulazione
009400120420     c                   movel     'S'           bs39sim
009500120420     c                   movel     v1cemail      bs39email
009600120420     c                   movel     tibs39ds      kpjbu
009700120420     c                   call      'TIBS39R'
009800120420     c                   parm                    KPJBA
009900120420     c
010000120420     c                   goto      for01
010100120420     c                   endif
010200120420     c
010300120420     c* F13 -  fine con aggiornamento profili
010400120420     c                   if        *inkm
010500120420     c                   clear                   kpjbu
010600120420     c                   clear                   tibs39ds
010700120420     c* solo aggiunta
010800120420     c                   movel     'S'           bs39agg
010900120420     c                   movel     v1cemail      bs39email
011000120420     c                   movel     tibs39ds      kpjbu
011100120420     c                   call      'TIBS39R'
011200120420     c                   parm                    KPJBA
011300120420     c
011400120420     C                   EXSR      F03Ges
011500120420     c                   goto      endv1
011600120420     c                   endif
011700020208      *
011800020208      * Controlli e decodifiche
011900020208     C                   EXSR      CtrV1
012000070126     c* Errori
012100070126     c                   if        *in90
012200070126     c                   goto      for01
012300070126     c                   endif
012400070126     c
012500070126     c                   movel     '2'           tipvid            1
012600020208      *
012700070126     C     endv1         endsr
012800020208      *--------------------------------------------------------------------------------------------*
012900020208      * Controllo e decodifica prima videata
013000020208      *--------------------------------------------------------------------------------------------*
013100020208     C     CtrV1         BEGSR
013200020208      *
013300020214     C                   setoff                                       283940
013400020208     C                   clear                   V1CMsg
013500120419     c
013600120419     C     '?'           SCAN      V1Clib                                 90
013700120419      *  Ricerca
013800120419    1C                   IF        *IN90 = *ON
013900120419     c                   clear                   tibs02ds
014000120419     c                   movel     'R'           t02mod
014100120419     c                   movel     knsif         t02sif
014200120419     C                   movel     'CUR'         t02cod
014300120419     C                   CALL      'TIBS02R'
014400120419     c                   parm                    KPJBA
014500120419     c                   parm                    TIBS02DS
014600120419      *
014700120419    2C                   IF        T02err = *blanks
014800120419     C                   movel     T02ke1        V1Clib
014900120419     C                   ENDIF
015000120419     c                   leavesr
015100120419     C                   ENDIF
015200070126      *
015300120418      * La libreria deve iniziare per GAICUR e deve esistere
015400120418     c                   if        %subst(v1clib:1:6)<>'GAICUR'
015500120418     c                   seton                                        409028
015600120418     C                   MOVEL     MSG(2)        V1cMSG
015700120418     c                   goto      endct1
015800120418     c                   endif
015900120418     c* controllo se esiste
016000120418     C                   Z-ADD     40            LUNG             15 5
016100120418     C                   MOVEa     CMDchk        COMMAN           80
016200120418     c                   eval      %subst(comman:13:10) = v1clib
016300120418     C                   CALL      'QCMDEXC'                            90
016400120418     C                   PARM                    COMMAN
016500120418     C                   PARM                    LUNG
016600120418     C** Se non esiste eseguo addpfm x membro nuovo
016700120418     C     *IN90         IFEQ      '1'
016800120418     c                   seton                                        4028
016900120418     C                   MOVEL     MSG(2)        V1cMSG
017000120418     c                   goto      endct1
017100120418     c                   endif
017200070126     c
017300070126     C     endct1        ENDSR
017400070126     C**************************************************************************
017500070126     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
017600070126     C**************************************************************************
017700070126     C     CTRFIL        BEGSR
017800070126
017900070126     C     CODFIL        CHAIN     AZORG01L                           34
018000070126    1C  N34ORGFVA        IFNE      ' '
018100070126     C     ORGFAG        ORNE      'A'
018200070126     C     ORGFAG        ANDNE     'F'
018300070126     C                   SETON                                        34
018400070126    1C                   ENDIF
018500070126     c
018600070126    1c                   if        not *in34
018700070126     c                   movel     orgdes        V1Dfil
018800070126     c
018900070126   x1C                   ELSE
019000120418     c                   seton                                        419028
019100070126     C                   MOVEL     MSG(1)        V1cMSG
019200070126     c                   goto      endctf
019300070126    1C                   ENDIF
019400070126     C*
019500070126     C     endctf        ENDSR
019600020208      *--------------------------------------------------------------------------------------------*
019700020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
019800020208      *--------------------------------------------------------------------------------------------*
019900020208     C     GesVd2        BEGSR
020000020208      *
020100020208     C                   EXSR      CarVd2
020200020208      *
020300070126     c     for02         tag
020400120418     C                   EXFMT     Tb94v2
020500070126     c
020600070126     c                   clear                   v1cmsg
020700070126     c                   setoff                                       2890
020800020208      *
020900070126     C* F3 - fine lavoro
021000070126     c                   if        *inkc
021100070126     C                   EXSR      F03Ges
021200070126     c                   goto      endv2
021300070126     c                   endif
021400070126      *
021500070126     C* F12 - ritorno
021600070126     c                   if        *inkl
021700070126     C                   move      '1'           TipVid
021800070126     c                   goto      endv2
021900070126     c                   endif
022000070126      *
022100020208     C                   EXSR      CtrV2
022200070126     c
022300120420     c                   if        *in90  or
022400120420     c                             (not *inkf and not *inkq)
022500070126     c                   goto      for02
022600070126     c                   endif
022700070126     c
022800120418     c* f6 - Aggiorna    - F16 annulla
022900120418     c                   if        *inkf  or *inkq
023000070126     c                   exsr      Aggiorna
023100070126     c                   endif
023200070129     c
023300070129     c                   movel     '1'           tipvid            1
023400120418     c                   clear                   v1clib
023500020211      *
023600070126     C     Endv2         endsr
023700020208      *--------------------------------------------------------------------------------------------*
023800020208      * CarVd2 - Carica primo video
023900020208      *--------------------------------------------------------------------------------------------*
024000020208     C     CarVd2        BEGSR
024100120418     c                   clear                   v1cfil
024200120418     c                   clear                   v1dfil
024300120418     c                   movel     'F'           v1capl
024400120418     c                   clear                   v1cdde
024500120420     c                   setoff                                       404142
024600020208      *
024700020211      * Leggo la tabella
024800120418     C                   movel(P)  v1clib        TbeKe1
024900020211     C                   movel     *blanks       TbeKe2
025000070126     C     ktbe          chain     tntbe01l                           21
025100070126     c
025200070126     c* Se trovata carico i dati
025300120418     c                   if        %found(tntbe01l) and tbeatb=*blanks
025400120418     c                   movel     tbeuni        dcur
025500070126     c                   eval      v1cmod='VARIAZIONE '
025600070126     c
025700120418     c                   movel     §curfil       v1cfil
025800120418     c     v1cfil        chain     azorg01l
025900120418     c                   if        %found(azorg01l)
026000120418     c                   movel     orgdes        v1dfil
026100120418     c                   endif
026200120418     c
026300120418     c                   movel     §curdde       dataiso
026400120418     c                   movel     dataiso       dataeur
026500120418     c                   movel     dataeur       v1cdde
026600120418     c
026700120418     c                   movel     §curapl       v1capl
026800120418     c                   else
026900120418     c                   clear                   dcur
027000120418     c                   eval      v1cmod='INSERIMENTO'
027100120418     c                   endif
027200020208     C                   ENDSR
027300020208      *--------------------------------------------------------------------------------------------*
027400020215      * Controllo seconda videata
027500020208      *--------------------------------------------------------------------------------------------*
027600020208     C     CtrV2         BEGSR
027700070126     C                   setoff                                       289041
027800070126     C                   setoff                                       424344
027900120418     c                   clear                   amgdde
028000120418      * PUNTO OPERATIVO
028100120418     C                   MOVE      V1Cfil        CODFIL            3 0
028200120418     C                   EXSR      CTRFIL
028300120418    1c                   if        *in90
028400120418     c                   goto      endct2
028500120418    1C                   ENDIF
028600120418     c
028700120418      * Immettere una data valida
028800120418     C                   Z-ADD     V1cdde        G02DAT
028900120418     C                   MOVEL     *BLANK        G02ERR
029000120418     C                   CALL      'XSRDA8'
029100120418     C                   PARM                    WLBDAT
029200120418     C* ERRATA
029300120418    1C     G02ERR        IFEQ      '1'
029400120418     c                   seton                                        422890
029500120418     C                   MOVEL     MSG(3)        V1cMSG
029600120418     c                   goto      endct2
029700120418     c                   endif
029800120418     c
029900120418     c                   movel     g02dat        v1cDDE
030000120418     c                   movel     g02inv        AMGDDE            8 0
030100120419     c* se richiesta la filiale come terminal, deve esistere alla data immessa
030200120419     c                   if        v1capl='T'
030300120418     c                   clear                   fnlv55ds
030400120418     c                   eval      d55tpt = 'P'
030500120418     c                   z-add     v1cfil        d55lin
030600120418     c                   z-add     amgdde        d55drf
030700120418     c                   call      'FNLV55R'
030800120418     c                   parm                    fnlv55ds
030900120418     c
031000120418     c                   if        d55err <>' ' or d55tfp<>v1cfil
031100120418     c                   seton                                        412890
031200120418     C                   MOVEL     MSG(4)        V1cMSG
031300120418     c                   goto      endct2
031400120418     c                   endif
031500120419     c                   endif
031600120418     c
031700070126     C     endct2        ENDSR
031800020208      *--------------------------------------------------------------------------------------------*
031900070126      * Aggiornamenti
032000020208      *--------------------------------------------------------------------------------------------*
032100070126     C     Aggiorna      BEGSR
032200020211      *
032300070126     C                   clear                   Tntbe000
032400020211     C                   move      ' '           TbeAtb
032500020211     C                   movel     $Sif          TbeSif
032600020211     C                   movel     $Lin          TbeLin
032700020211     C                   movel     $Apl          TbeApl
032800120418     C                   move      'CUR'         TbeCod
032900020211     C                   move      *blanks       TbeKe1
033000020211     C                   move      *blanks       TbeKe2
033100120418     c                   movel     v1clib        tbeke1
033200070126     c* campi della DS
033300120418     c                   clear                   dcur
033400120418     c                   movel     v1clib        §curlib
033500120418     c                   movel     v1capl        §curapl
033600120418     c                   movel     v1cfil        §curfil
033700120418     c                   movel     amgdde        §curdde
033800120418     C                   movel(P)  dcur          TbeUni
033900020215      *
034000120420     C                   move      'S'           TbeFtt
034100020211     C                   z-add     *zeros        TbeFlt
034200020211     C                   move      ' '           TbeFtr
034300020211     C                   move      *zeros        TbeDtr
034400120418     c                   if        *inkq
034500120418     c                   eval      tbeatb='A'
034600120418     c                   endif
034700020211      *
034800070126     c                   if        %found(tntbe01l)
034900070126     c                   update    tntbe000
035000070126     c                   else
035100120418     c                   if        not *inkq
035200070126     C                   write     Tntbe000
035300070126     c                   endif
035400120418     c                   endif
035500020211      *
035600020211     C                   ENDSR
035700020208      *--------------------------------------------------------------------------------------------*
035800020208      * F03ges - Tasto funzionale f03 -> Fine programma
035900020208      *--------------------------------------------------------------------------------------------*
036000020208     C     F03ges        BEGSR
036100070126     C                   movel     'S'           $Fine             1            *fine pgm
036200020208     C                   ENDSR
036300120420      *--------------------------------------------------------------------------------------------*
036400120420      * controllo indirizzo e-mail
036500120420      *--------------------------------------------------------------------------------------------*
036600120420     C     Ctremail      BEGSR
036700120420     c
036800120420     C                   if        v1cemail<>*blanks
036900120420     c                   clear                   dsemail
037000120420     c                   eval      §EMLINDI=%trim(v1cemail)+'@brt.it'
037100120420     c                   call      'XEMAIL'
037200120420     c                   parm                    dsemail
037300120420     c
037400120420     c                   if        §EMLERRO<>*blanks and §EMLERRO<> '0'
037500120420     c                   seton                                        412890
037600120420     C                   MOVEL     §EMLMSGO      V1cMSG
037700120420     c                   leavesr
037800120420     c                   endif
037900120420     c                   endif
038000120420     c
038100120420     c                   if        v1cemail=*blanks
038200120420     c                   seton                                        412890
038300120420     C                   MOVEL     msg(5)        V1cMSG
038400120420     c                   leavesr
038500120420     c                   endif
038600120420     c
038700120420     C                   ENDSR
038800020208      *--------------------------------------------------------------------------------------------*
038900020208      * RutInz - operazioni iniziali
039000020208      *--------------------------------------------------------------------------------------------*
039100020208     C     rutinz        BEGSR
039200020208      *
039300020208      * Ricevimento parametri
039400020208     C     *ENTRY        PLIST
039500020208     C                   PARM                    kpjba
039600020208      *
039700020208     C                   z-add     1             CodUt
039800020208     C                   CALL      'X§PARUT'
039900020208     C                   PARM                    UT§DSE
040000020208     C                   move      '1'           TipVid
040100020211      *
040200120418      * Reperisco testata tabella
040300120418     C                   movel     *blanks       Tbecod
040400120418     C                   move      *zeros        TbeKe1
040500120418     C                   move      'CUR'         TbeKe1
040600120418     C     K01Tbe2       chain     Tntbe01l                           21
040700120418      *
040800120418      * Se non esiste l atestata tabella non faccio nulla
040900120418     C   21              move      '1'           $Fine
041000120418      *
041100120418     c                   if        not *in21
041200020211     C                   movel     TbeSif        $Sif             10
041300020211     C                   movel     TbeApl        $Apl              2
041400020211     C                   movel     TbeLin        $Lin              1
041500120418     C                   move      'CUR'         TbeCod
041600120418     c                   endif
041700070126     c
041800070126     c* EDP tutto è possibile
041900070126     c                   if        %subst(knmus:1:3)='EDP'
042000070126     c                   seton                                        01
042100070126     c                   endif
042200070126     c                   movel     ragut         v1crag
042300070126     c                   movel     knmus         v1cute
042400070126     c                   movel     knsif         v1csif
042500020208      *
042600020208      * chiavi di lettura
042700020208      *
042800020211     C     k01tbe2       KLIST                                                  *tntbe01l
042900020211     C                   KFLD                    tbecod                          -tabella
043000020211     C                   KFLD                    tbeke1                          -chiave uno
043100020212      *
043200070126     C     ktbe          KLIST                                                  *tntbe01l
043300020212     C                   KFLD                    TbeCod                          -tabella
043400020212     C                   KFLD                    tbeke1                          -chiave uno
043500020212     C                   KFLD                    tbeKe2                          -tabella
043600020212     C                   KFLD                    $lin                            -chiave uno
043700020212     C                   KFLD                    $sif                            -tabella
043800070126     c                   ENDSR
043900020212      *
044000020211      *
044100020212** Msg
044200120418Filiale inesistente o errata
044300120418La libreria deve essere una GAICURxxx esistente
044400120418data errata
044500120418La filiale immessa non è un terminal di partenza alla data indicata
044600120420Se premuto F10 o F13 immettere indirizzo e-amil BRT
044700120418**         CMDchk
044800120418CHKOBJ  OBJ(GAICURxxx ) OBJTYPE(*LIB)
