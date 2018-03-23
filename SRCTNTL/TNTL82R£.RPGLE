000100080912     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080912      *
000300081031     Ftntl82d   CF   E             WORKSTN
000400080912     f                                     sfile(video2:snrr1)
000500120605     Faiats05l  uF a E           k disk
000600081016     Faiats02l  iF   E           k disk    rename(aiats000:aiats2)
000700081016     Fanfrn01l  iF   E           k disk
000800081016     Fansog01l  iF   E           k disk
000900090713     Ftabel00f  iF   E           k disk
001000081103     Faiata01l  uF a E           k disk
001100081103     Faiatm01l  uF a E           k disk
001200080912      *
001300080912     D Psds           SDS
001400080912     D  PgmName          *PROC
001500080912     D Kpjba         E DS
001600081028     d param           ds
001700081028     d  parsoc                        3s 0
001800081028     d  parksc                        7s 0
001900081028     d  parmod                        1
002000081031     d paramb          ds
002100081031     d  parsocb                       3s 0
002200081031     d  paroldcb                      7s 0
002300081031     d  parnewcb                      7s 0
002400081031     d  parmodb                       1
002500080912      *
002600120528     d partar          ds
002700120528     d  parnrc                        7s 0
002800120528     d  pardt                         8s 0
002900120528     d  esito                         1
003000120604      *
003100090713     D trmz70s1ds    E DS                  prefix(S1_)
003200080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003300080912     D CNCR80        E DS
003400120601     d Trul33Ds      e ds
003500080912     d Tibs36Ds      e ds
003600100108     d ds3i          e ds
003700080912     d data            ds            10
003800080912     d aa                      3      4
003900080912     d mm                      6      7
004000080912     d gg                      9     10
004100080912      *
004200080912     D fatto           S              1
004300080912     D modalita        S              1
004400080912     D x               S              4  0
004500080912     D WrkEofS01       S              1
004600080912     D WrkCarS01       S              1
004700080912     D $VIDEO          S             10
004800080912     D snrr1           S              5i 0
004900080912     D Savkpjbu        S                   like(kpjbu)
005000080912     D SavOpzione      S                   like(v1csce)
005100080912     d dataiso         s               d   datfmt(*iso)
005200080912     d dataeur         s               d   datfmt(*eur)
005300080912     D WLBDA8          DS
005400080912     D  G02DAT                 1      8  0
005500080912     D  G02INV                 9     16  0
005600080912     D  G02ERR                17     17
005700080912     D  G02TGI                18     22  0
005800081216
005900081216      * Controllo IVA
006000151117     d xcfiva1ds     e ds
006100081216     d  ivaeu                  3      4
006200081216     d  ivait                  5     18
006300080912      *---------------- ----------------------------------------------------
006400080912      *---------------- Gestione formato video video01----------------------
006500081118     c                   if        visualizza = *blank
006600081118     c                   seton                                        21
006700081118     c                   endif
006800080912     C                   EXSR      Inzvideo1
006900080912     C                   DOU       $Video <> 'VIDEO1'
007000080912      *----------- Visualizzo dati di output in caso di errori.-------------
007100080912     C                   IF        *IN99
007200080912     C                   EVAL      *IN99 = *OFF
007300080912     C                   WRITE     video1
007400080912     C                   EVAL      *IN99 = *ON
007500080912     C                   ENDIF
007600080912      *
007700080912     c     emetti        tag
007800080912     C                   EXFMT     video1
007900120608     c                   setoff                                       2896
008000120608     c                   clear                   $msg
008100080912      *
008200080912     C                   SELECT
008300080912     C                   WHEN      *INKC
008400080912     C                             OR
008500080912     C                             *INKL
008600080912     C                   EVAL      $Video = *BLANKS
008700080912
008800080912     C                   WHEN      *INKE
008900080912     C                   EXSR      Inzvideo1
009000080912     C                   WHEN      *INKB
009100080912     C                   EXSR      Chkvideo1
009200080912     C                   OTHER
009300080912     C                   EXSR      Chkvideo1
009400080912     c   84              goto      emetti
009500080912     C  N99              EXSR      Wrkvideo2c
009600080912     C                   ENDSL
009700080912      *
009800080912     C                   ENDDO
009900080912      *
010000080912     C                   EXSR      Uscita
010100080912     c**********************************************************************
010200080912     c* uscita
010300120524     c***********************************************\**********************
010400080912     C     Uscita        BEGSR
010500080912      *
010600080912     C                   EVAL      *INLR = *ON
010700080912     C                   RETURN
010800080912      *
010900080912     C                   ENDSR
011000080912     c**********************************************************************
011100080912     c* gestione sfl
011200080912     c**********************************************************************
011300080912     C     Wrkvideo2c    BEGSR
011400080912      *
011500080912     C                   EVAL      WrkCarS01 = *ON
011600080912     C                   EVAL      $Video = 'VIDEO2C'
011700080912      *
011800080912     C                   DOU       $Video <> 'VIDEO2C'
011900080912     C                   EXSR      Carvideo2
012000080912     c*Descrizione della ricerca
012100080912      *
012200080912     C                   WRITE     video2z
012300080912     C                   EXFMT     video2c
012400120608     c                   setoff                                       2896
012500120608     c                   clear                   $msg
012600080912     C                   SELECT
012700080912     c* fine
012800080912     C                   WHEN      *INKC=*ON
012900080912     C                   EVAL      $VIDEO=*BLANKS
013000080912     c* guida
013100080912     C                   WHEN      *INKL=*ON
013200080912     C                   EVAL      $VIDEO='VIDEO1'
013300080912     c* ripristino
013400080912     C                   WHEN      *INKE=*ON
013500080912     C                   EVAL      WrkCarS01 = *ON
013600080912
013700080912     C                   OTHER
013800080912
013900080912     C                   EXSR      GestioneSFL
014000080912     C                   ENDSL
014100080912
014200080912     C                   ENDDO
014300080912     C                   ENDSR
014400080912     c**********************************************************************
014500080912     c* Controlli video R01.
014600080912     c**********************************************************************
014700080912     C     Chkvideo1     BEGSR
014800080912      *
014900080912     C                   EVAL      *IN99 = *OFF
015000080912     C                   EVAL      *IN84 = *OFF
015100080912      * profilo di filiale
015200080912     c                   if        o36pou <> 046
015300080912     c                   endif
015400080912      *
015500080912     C                   ENDSR
015600080912     c**********************************************************************
015700080912     c* inizializza r01
015800080912     c**********************************************************************
015900080912     C     Inzvideo1     BEGSR
016000080912
016100080912     c                   if        o36pou <> 046
016200080912     c                   move      *blank        vrags
016300081118     c                   move      'N'           vdis
016400081118     c                   else
016500081118     c                   move      'S'           vdis
016600080912     c                   endif
016700080912     C                   EXSR      Chkvideo1
016800080912
016900080912     C                   ENDSR
017000080912     c**********************************************************************
017100080912     C     opzione       BEGSR
017200080912     c**********************************************************************
017300080912     c                   move      v1csce        modalita
017400081016     c                   movea     '00000000'    *in(50)
017500080912     c                   setoff                                       96
017600080912     c                   select
017700081118     c                   when      v1csce = '2' and visualizza <> *blank
017800080912     c                   exsr      modifica
017900081118     c                   when      v1csce = '3' and visualizza <> *blank
018000080912     c                   exsr      copia
018100081118     c                   when      v1csce = 'A' and visualizza <> *blank
018200081028     c                   exsr      autisti
018300081118     c                   when      v1csce = 'M' and visualizza <> *blank
018400081028     c                   exsr      automezzi
018500120509     c                   when      v1csce = 'N' and visualizza <> *blank
018600141001     c                   if        vi2nrc = 0
018700141001     c                   eval      $msg = 'Opzione N possibile solo se esiste -
018800141001     c                             un contratto precedente'
018900141001     c                   seton                                        9628
019000141001     c                   leavesr
019100141001     c                   endif
019200120604     c                   exsr      nuovocontr
019300081118     c                   when      v1csce = 'C' and visualizza <> *blank
019400120608     c*verifica esistenza tariffe valide
019500120608     c                   clear                   partar
019600120608     c                   move      vi2nrc        parnrc
019700120608     c                   move      wuda          pardt
019800120608     c                   call      'TRULTAR'
019900120608     c                   parm                    partar
020000120608     c                   if        esito = '1'
020100120608     c                   eval      $msg = 'Esistono tariffe valide legate al -
020200120608     c                             contratto cambio codice non possibile'
020300120608     c                   seton                                        9628
020400120608     c                   leavesr
020500120608     c                   endif
020600081031     c                   exsr      cambiocodice
020700120605     c                   when      v1csce = '7' and visualizza <> *blank
020800120605     c                   exsr      allegati
020900090528     c                   when      v1csce = 'T' and visualizza <> *blank
021000120608     c*verifica esistenza tariffe valide
021100120608     c                   clear                   partar
021200120608     c                   move      vi2nrc        parnrc
021300120608     c                   move      wuda          pardt
021400120608     c                   call      'TRULTAR'
021500120608     c                   parm                    partar
021600120608     c                   if        esito = '1'
021700120608     c                   eval      $msg = 'Esistono tariffe valide legate al -
021800120608     c                             contratto trasferimento non possibile'
021900120608     c                   seton                                        9628
022000120608     c                   leavesr
022100120608     c                   endif
022200090528     c                   exsr      trasferimento
022300081215     c                   when      v1csce = '5' and visualizza = *blank
022400080912     c                   seton                                        21
022500080912     c                   exsr      modifica
022600081118     c                   if        visualizza <> *blank
022700080912     c                   setoff                                       21
022800081118     c                   endif
022900080912     c                   endsl
023000080912     C                   ENDSR
023100080912     c**********************************************************************
023200080912     C     modifica      BEGSR
023300080912     c**********************************************************************
023400120605     c  n21katsd         chain     aiats05l                           98
023500120605     c   21katsd         chain(n)  aiats05l                           98
023600080912     c                   if        not *in98
023700080912     c                   move      ATSKSC        vATSKSC
023800080912     c                   move      ATSSOC        vATSSOC
023900080912     c                   move      ATSRAGS       vATSRAGS
024000080912     c                   move      ATSPIVA       vATSPIVA
024100120528     c                   move      ATSnrc        vATSnrc
024200120503      *data accreditamento
024300080912     c                   if        atsdtinc > 0
024400080912     c                   move      atsdtinc      dataiso
024500080912     c                   move      dataiso       dataeur
024600080912     c                   move      dataeur       vatsdtinc
024700080912     c                   else
024800080912     c                   clear                   vatsdtinc
024900080912     c                   endif
025000120503      *data disaccreditamento
025100080912     c                   if        atsdtfic > 0
025200080912     c                   move      atsdtfic      dataiso
025300080912     c                   move      dataiso       dataeur
025400080912     c                   move      dataeur       vatsdtfic
025500080912     c                   else
025600080912     c                   clear                   vatsdtfic
025700080912     c                   endif
025800120503      *data inizio contratto
025900120503     c                   if        atsdec  > 0
026000120503     c                   move      atsdec        dataiso
026100120503     c                   move      dataiso       dataeur
026200120503     c                   move      dataeur       vatsdec
026300120509     c                   seton                                        31
026400120503     c                   else
026500120509     c                   setoff                                       31
026600120503     c                   clear                   vatsdec
026700120503     c                   endif
026800120503      *data fine contratto
026900120605     c                   setoff                                       32
027000120503     c                   if        atsdfc   > 0
027100120503     c                   move      atsdfc        dataiso
027200120503     c                   move      dataiso       dataeur
027300120503     c                   move      dataeur       vatsdfc
027400120605     c                   if        atsdfc <> 20391231
027500120605     c                   seton                                        32
027600120605     c                   endif
027700120503     c                   else
027800120503     c                   clear                   vatsdfc
027900120503     c                   endif
028000120419      *data  contratto
028100120426     c                   if        atsdrc > 0
028200120426     c                   move      atsdrc        dataiso
028300120419     c                   move      dataiso       dataeur
028400120426     c                   move      dataeur       vatsdrc
028500120419     c                   else
028600120426     c                   clear                   vatsdrc
028700120419     c                   endif
028800090710      **
028900090710      ** Nuovi campi Registro Iscrizione all'albo e data
029000090713     c********           eval      vatCIT = atSCIT
029100090713     c********           eval      vatNIS = atSNIS
029200090713
029300090710     c                   eval      vatPIA = atSPIA
029400090713      ** ----------
029500090713      **  Routine x Reperire ulteriori dati anagrafici Fornitore:
029600090713     c                   if        vatPIA = *blank
029700090713     C                   clear                   trmz70s1ds                     Input
029800090713     C                   movel(p)  atsPIVA       s1_PartitaIVA                  Input
029900090713     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
030000090713     C                   movel(p)  atsSOC        s1_Societa                     Input/Output
030100090713     C                   move      *all'0'       s1_KeyKSC                      Input/Output
030200090713     C                   move      atsKSC        s1_KeyKSC                      Input/Output
030300090713     c                   call      'TRMZ70SR1'
030400090713     C                   PARM                    trmz70s1ds                     Input
030500090713     c                   if        s1_errore ='0'
030600090713     c                   eval      vatPIA = S1_Provincia
030700090713     c                   end
030800090713     c                   end
030900090713      ** ----------
031000090710     c                   eval      vatNIA = atSNIA
031100090710
031200090710     c                   if        atsdia > 0
031300090710     c                   move      atsDia        dataiso
031400090710     c                   move      dataiso       dataeur
031500090710     c                   move      dataeur       vatDia
031600090713     c                   else
031700090713     c                   clear                   vatDia
031800090710     c                   endif
031900090710      **
032000080912     c                   exsr      Gesvideo3
032100080912     c                   endif
032200080912     C                   ENDSR
032300080912     c**********************************************************************
032400080912     C     copia         BEGSR
032500080912     c**********************************************************************
032600080912     C                   ENDSR
032700081028     c**********************************************************************
032800081028     C     automezzi     BEGSR
032900081028     c**********************************************************************
033000081028     c                   clear                   param
033100081028     c                   z-add     vi2soc        parsoc
033200081028     c                   z-add     vi2ksc        parksc
033300081028     c                   movel     param         kpjbu
033400081031     c                   call      'TNTL84R'
033500081028     C                   parm                    kpjba
033600081028     C                   ENDSR
033700081031     c**********************************************************************
033800081031     C     cambiocodice  BEGSR
033900081031     c**********************************************************************
034000081031     c                   clear                   paramb
034100081031     c                   z-add     vi2soc        parsocb
034200081031     c                   z-add     vi2ksc        paroldcb
034300081031     c                   movel     paramb        kpjbu
034400081031     c                   call      'TNTL85R'
034500081031     C                   parm                    kpjba
034600081031     C                   ENDSR
034700120509     c**********************************************************************
034800120509     C     nuovocontr    BEGSR
034900120509     c**********************************************************************
035000120509     c                   clear                   paramb
035100120509     c                   z-add     vi2soc        parsocb
035200120509     c                   z-add     vi2ksc        paroldcb
035300120509     c                   movel     paramb        kpjbu
035400120509     c                   call      'TNTL86R'
035500120509     C                   parm                    kpjba
035600120509     C                   ENDSR
035700120605     c**********************************************************************
035800120605     C     allegati      BEGSR
035900120605     c**********************************************************************
036000120605     c                   clear                   partar
036100120606     c                   if        vi2nrc > 0
036200120605     c                   move      vi2nrc        parnrc
036300120605     c                   move      wuda          pardt
036400120605     c                   call      'TNTL87R'
036500120605     c                   parm                    partar
036600120606     c                   endif
036700120605     C                   ENDSR
036800090528     c**********************************************************************
036900090528     C     trasferimento BEGSR
037000090528     c**********************************************************************
037100090528     c                   clear                   paramb
037200090528     c                   z-add     vi2soc        parsocb
037300090528     c                   z-add     vi2ksc        paroldcb
037400090528     c                   move      'T'           parmodb
037500090528     c                   movel     paramb        kpjbu
037600090528     c                   call      'TNTL85R'
037700090528     C                   parm                    kpjba
037800090528     C                   ENDSR
037900081028     c**********************************************************************
038000081028     C     autisti       BEGSR
038100081028     c**********************************************************************
038200081028     c                   clear                   param
038300081028     c                   z-add     vi2soc        parsoc
038400081028     c                   z-add     vi2ksc        parksc
038500081028     c                   movel     param         kpjbu
038600081031     c                   call      'TNTL83R'
038700081028     C                   parm                    kpjba
038800081028     C                   ENDSR
038900080912     c**********************************************************************
039000080912     C     gesvideo3     BEGSR
039100080912     c**********************************************************************
039200080912     c                   clear                   fatto
039300080912     c                   do        *hival
039400080912     c                   exfmt     video3
039500120605     c                   setoff                                       28
039600120605     c                   clear                   $msg
039700090826     c   kl              if        not *in21
039800120605     c                   unlock    aiats05l
039900090826     c                   end
040000090826     c   kl              leave
040100080912     c  n21              exsr      controv3
040200080912     c   96              iter
040300080912     c   kf              exsr      aggiorna
040400080912     c                   if        fatto <> *blank
040500080912     c                   leave
040600080912     c                   endif
040700080912
040800080912     c                   enddo
040900080912     C                   ENDSR
041000080912     c**********************************************************************
041100080912     C     Controv3      BEGSR
041200080912     c**********************************************************************
041300080912      *
041400080912      * *in96 errore generico videata dettaglio
041500080912     c                   setoff                                       9628
041600080912     c                   clear                   $msg
041700080912     c                   if        modalita ='4'
041800080912     c                   goto      endctr
041900080912     c                   endif
042000081016      *codice fornitore
042100081016     c                   setoff                                       58
042200081016     c                   if        vatsksc =  *zeros
042300081016     c                   seton                                        5896
042400080912     c                   endif
042500081016     c                   setoff                                       59
042600081016     c                   movel     vatssoc       societa
042700081016     c                   movel     *zeros        codice
042800081016     c                   move      vatsksc       codice
042900081016     c     kfrn          chain     anfrn01l
043000081016     c                   if        not%found(anfrn01l)
043100081016     c                   seton                                        5996
043200081016     c                   else
043300081016     c     frnsogg       chain     ansog01l
043400081016     c                   movel     sogdes        vatsrags
043500081016     c                   endif
043600081016     c
043700081016     c                   setoff                                       57
043800081016      *codice partita iva obbligatori
043900081103     c                   if        vatspiva =  *blank
044000081016     c                   seton                                        5796
044100081216     c                   goto      endctr
044200081016     c                   endif
044300081216      *controllo formale partita iva
044400081216     c                   setoff                                       55
044500081216      *  richiama il nuovo driver fatto per controllare il codice
044600081216      * fiscale o la partita iva
044700151117     c                   clear                   xcfiva1ds
044800081216     c                   eval      xcfivapar = vatspiva
044900151117     c                   call      'XCFIVAR1'
045000151117     c                   parm                    xcfiva1ds
045100081216      * --> errore
045200081216     c                   if        XCFIVAFLG < *zeros
045300081216     c                   seton                                        5596
045400081216     c                   goto      endctr
045500081216     c                   EndIf
045600081216      *controllo corrispondenza partita iva immessa con partita iva del fornitore
045700081216     c                   setoff                                       56
045800081216     c                   if        sogpartiva <> vatspiva
045900081216     c                   seton                                        5696
046000081216     c                   goto      endctr
046100081216     c                   endif
046200120420      *data accreditamento obbligatoria
046300080912     c                   setoff                                       53
046400080912     c                   if        vatsdtinc =  *zeros
046500080912     c                   seton                                        5396
046600120809     c                   goto      endctr
046700080912     c                   endif
046800120420      *controlli data  accreditamento
046900080912     c                   setoff                                       50
047000080912     c                   if        vatsdtinc > 0
047100080912     C                   MOVE      Vatsdtinc     G02DAT
047200080912     C                   MOVEL     *BLANK        G02ERR
047300080912     C                   CALL      'XSRDA8'
047400080912     C                   PARM                    WLBDA8
047500080912     C     G02ERR        IFEQ      '1'
047600080912     C                   SETON                                        5096
047700120809     c                   goto      endctr
047800080912     C                   END
047900080912     c                   move      g02dat        vatsdtinc
048000080912     c                   move      g02inv        vatsdtincg        8 0
048100080912     c                   endif
048200120420      *controlli data  disaccreditamento
048300081016     c                   setoff                                       51
048400080912     c                   if        vatsdtfic > 0
048500080912     C                   MOVE      Vatsdtfic     G02DAT
048600080912     C                   MOVEL     *BLANK        G02ERR
048700080912     C                   CALL      'XSRDA8'
048800080912     C                   PARM                    WLBDA8
048900080912     C     G02ERR        IFEQ      '1'
049000081016     C                   SETON                                        5196
049100120809     c                   goto      endctr
049200080912     C                   END
049300080912     c                   move      g02dat        vatsdtfic
049400080912     c                   move      g02inv        vatsdtficg        8 0
049500120528     c*verifica esistenza tariffe valide
049600120528     c                   clear                   partar
049700120611     c                   if        vatsnrc > 0
049800120528     c                   move      vatsnrc       parnrc
049900120528     c                   move      vatsdtficg    pardt
050000120605     c                   call      'TRULTAR'
050100120605     c                   parm                    partar
050200120605     c                   if        esito = '1'
050300120604     c                   call      'TNTL87R'
050400120604     c                   parm                    partar
050500120528     c                   eval      $msg = 'Esistono tariffe valide legate al -
050600120528     c                             contratto'
050700120528     c                   seton                                        9628
050800120809     c                   goto      endctr
050900120528     c                   leavesr
051000120528     c                   endif
051100120611     c                   endif
051200120809     c                   if        vatsdfc = 0 and vatsdec > 0 or
051300120809     c                             vatsdfc = 31122039 and vatsdec > 0
051400120509     c                   z-add     vatsdtfic     vatsdfc
051500120509     c                   endif
051600080912     c                   endif
051700120419      *controlli data  contratto
051800120419     c                   setoff                                       52
051900120426     c                   if        vATSdec  > 0
052000120426     C                   MOVE      VATSdec       G02DAT
052100120419     C                   MOVEL     *BLANK        G02ERR
052200120419     C                   CALL      'XSRDA8'
052300120419     C                   PARM                    WLBDA8
052400120419     C     G02ERR        IFEQ      '1'
052500120419     C                   SETON                                        5296
052600120809     c                   goto      endctr
052700120419     C                   END
052800120426     c                   move      g02dat        vATSdec
052900120426     c                   move      g02inv        vATSdecg          8 0
053000120426      * data inizio contratto >= data accreditamento
053100120426     c                   if        vatsdtincg > vatsdecg
053200120420     c                   seton                                        5296
053300120809     c                   goto      endctr
053400120420     c                   endif
053500120903      * data disaccreditamento < data contratto
053600120903     c                   if        vatsdtficg > 0  and
053700120903     c                             vatsdtficg < vatsdecg
053800120903     c                   seton                                        5196
053900120903     c                   goto      endctr
054000120903     c                   endif
054100120419     c                   endif
054200120426      * data rientro copia firmata
054300120426     c                   setoff                                       54
054400120426     c                   if        vATSdrc  > 0
054500120426     C                   MOVE      VATSdrc       G02DAT
054600120426     C                   MOVEL     *BLANK        G02ERR
054700120426     C                   CALL      'XSRDA8'
054800120426     C                   PARM                    WLBDA8
054900120426     C     G02ERR        IFEQ      '1'
055000120426     C                   SETON                                        5496
055100120809     c                   goto      endctr
055200120426     C                   END
055300120426     c                   move      g02dat        vATSdrc
055400120813     c                   move      g02inv        vATSdrcg          8 0
055500120813     c                   if        vatsdrcg  < vatsdecg
055600120813     c                   seton                                        5496
055700120813     c                   goto      endctr
055800120813     c                   endif
055900120903     c                   endif
056000120426      * data fine contratto
056100120426     c                   setoff                                       60
056200120605     c                   if        vATSdfc  > 0
056300120426     C                   MOVE      VATSdfc       G02DAT
056400120426     C                   MOVEL     *BLANK        G02ERR
056500120426     C                   CALL      'XSRDA8'
056600120426     C                   PARM                    WLBDA8
056700120426     C     G02ERR        IFEQ      '1'
056800120426     C                   SETON                                        6096
056900120809     c                   goto      endctr
057000120426     C                   END
057100120426     c                   move      g02dat        vATSdfc
057200120503     c                   move      g02inv        vATSdfcg          8 0
057300120528     c*verifica esistenza tariffe valide
057400120605     c                   if        vATSdfcg <> 20391231
057500120528     c                   clear                   partar
057600120611     c                   if        vatsnrc > 0
057700120528     c                   move      vatsnrc       parnrc
057800120528     c                   move      vatsdfcg      pardt
057900120528     c                   call      'TRULTAR'
058000120528     c                   parm                    partar
058100120528     c                   if        esito = '1'
058200120604     c                   call      'TNTL87R'
058300120604     c                   parm                    partar
058400120528     c                   eval      $msg = 'Esistono tariffe valide legate al -
058500120528     c                             contratto'
058600120528     c                   seton                                        9628
058700120809     c                   goto      endctr
058800120528     c                   leavesr
058900120528     c                   endif
059000120611     c                   endif
059100120605     c                   endif
059200120503     c                   if        vatsdfcg  < vatsdecg
059300120503     c                   seton                                        6096
059400120809     c                   goto      endctr
059500120503     c                   endif
059600120426     c                   endif
059700090710      *
059800090713      ** ----------
059900090713      **  Routine x Reperire ulteriori dati anagrafici Fornitore:
060000090713     C                   clear                   trmz70s1ds                     Input
060100090713     C                   movel(p)  sogpartiva    s1_PartitaIVA                  Input
060200090713     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
060300090713     C                   movel(p)  frnSOCIETA    s1_Societa                     Input/Output
060400090713     C                   movel(p)  frnUNITA      s1_Unita                       Input/Output
060500090713     C                   movel(p)  frnKSC        s1_KeyKSC                      Input/Output
060600090713     c                   call      'TRMZ70SR1'
060700090713     C                   PARM                    trmz70s1ds                     Input
060800090710      *
060900090710      *   Iscr.ALBO trasp.
061000090710     c                   If        vatpia  =  *blank
061100090713     c                   eval      vatPIA = S1_Provincia
061200090710     C                   ENDIF
061300090713     C*
061400090710      *   Data Iscriz.Albo
061500090710     c                   clear                   hvatDia           8 0
061600090710     c                   setoff                                       66
061700090710     c                   if        vatdia > 0
061800090710     C                   MOVE      Vatdia        G02DAT
061900090710     C                   MOVEL     *BLANK        G02ERR
062000090710     C                   CALL      'XSRDA8'
062100090710     C                   PARM                    WLBDA8
062200090710     C     G02ERR        IFEQ      '1'
062300090710     C                   SETON                                        6696
062400120809     c                   goto      endctr
062500090710     C                   END
062600090710     c                   move      g02dat        vatdia
062700090710     c                   move      g02inv        hvatDia           8 0
062800090713     c                   endif
062900090710      *
063000120813     C     endctr        ENDSR
063100080912     c**********************************************************************
063200080912     C     aggiorna      BEGSR
063300080912     c**********************************************************************
063400081016     c                   move      vATSRAGS      ATSRAGS
063500081016     c                   move      vatspiva      atspiva
063600081016     c                   if        vatsdtinc > 0
063700081016     c                   move      vatsdtinc     dataeur
063800080912     c                   move      dataeur       dataiso
063900081016     c                   move      dataiso       atsdtinc
064000080912     c                   endif
064100081016     c                   if        vatsdtfic > 0
064200081016     c                   move      vatsdtfic     dataeur
064300081016     c                   move      dataeur       dataiso
064400081016     c                   move      dataiso       atsdtfic
064500091230     c                   exsr      disaccrfigli
064600090109     c                   else
064700090109     c                   z-add     0             atsdtfic
064800081016     c                   endif
064900120426     c                   if        vatsdec > 0
065000120426     c                   move      vatsdec       dataeur
065100120426     c                   move      dataeur       dataiso
065200120426     c                   move      dataiso       atsdec
065300120426     c                   else
065400120426     c                   z-add     0             atsdec
065500120426     c                   endif
065600120426      **
065700120426     c                   if        vatsdfc > 0
065800120426     c                   move      vatsdfc       dataeur
065900120426     c                   move      dataeur       dataiso
066000120426     c                   move      dataiso       atsdfc
066100120426     c                   else
066200120611     c                   if        vatsdtfic > 0 and vatsdec > 0
066300120509     c                   move      vatsdtfic     dataeur
066400120509     c                   move      dataeur       dataiso
066500120509     c                   move      dataiso       atsdfc
066600120611     c                   endif
066700120611     c                   endif
066800120611     c                   if        atsdfc = 0 and vatsdec > 0
066900120608     c                   z-add     20391231      atsdfc
067000120426     c                   endif
067100120426      **
067200120426     c                   if        vatsdrc > 0
067300120426     c                   move      vatsdrc       dataeur
067400120419     c                   move      dataeur       dataiso
067500120426     c                   move      dataiso       atsdrc
067600120419     c                   else
067700120426     c                   z-add     0             atsdrc
067800120419     c                   endif
067900090710      **
068000090710      ** Nuovi campi Registro Iscrizione all'albo e data
068100090713     c********           eval       atSCIT  =  vatCIT
068200090713     c********           eval       atSNIS  =  vatNIS
068300090713
068400090710     c                   eval       atSPIA  =  vatPIA
068500090710     c                   eval       atSNIA  =  vatNIA
068600090710
068700090710     c                   if        vatdia > 0
068800090710     c                   move      vatdia        dataeur
068900090710     c                   move      dataeur       dataiso
069000090710     c                   move      dataiso       atsdia
069100090710     c                   else
069200090710     c                   z-add     0             atsdia
069300090710     c                   endif
069400090710      **
069500080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
069600080912     c                   select
069700081103      * se in aggiornamento la modalità è di cancellazione dell'anagrafica
069800081103      *principale elimina anche i rekord automezzi e autisti legati
069900080912     c                   when      modalita = '4'
070000081103     c                   exsr      eliminafigli
070100081016     c                   delete    aiats000
070200081103      *
070300080912     c                   when      modalita = *blank or modalita = '3'
070400081028     c                   move      vATSKSC       ATSKSC
070500081028     c                   move      vATSSOC       ATSSOC
070600081016     c                   write     aiats000
070700080912     c                   other
070800081016     c                   update    aiats000
070900080912     c                   endsl
071000080912     c                   move      'X'           fatto
071100080912     C                   ENDSR
071200081103     c**********************************************************************
071300081103     C     eliminafigli  BEGSR
071400081103     c**********************************************************************
071500081103      * elimina autisti legati
071600081103     c     kats          setll     aiata01l
071700081103     c                   do        *hival
071800081103     c     kats          reade     aiata01l
071900081103     c                   if        %eof(aiata01l)
072000081103     c                   leave
072100081103     c                   endif
072200081103     c                   delete    aiata000
072300081103     c                   enddo
072400081103      * elimina automezzi legati
072500081103     c     kats          setll     aiatm01l
072600081103     c                   do        *hival
072700081103     c     kats          reade     aiatm01l
072800081103     c                   if        %eof(aiatm01l)
072900081103     c                   leave
073000081103     c                   endif
073100081103     c                   delete    aiatm000
073200081103     c                   enddo
073300081103     C                   ENDSR
073400091230     c**********************************************************************
073500091230     C     disaccrfigli  BEGSR
073600091230     c**********************************************************************
073700091230      * disaccredita  autisti legati
073800091230     c     kats2         setll     aiata01l
073900091230     c                   do        *hival
074000091230     c     kats2         reade     aiata01l
074100091230     c                   if        %eof(aiata01l)
074200091230     c                   leave
074300091230     c                   endif
074400091230     c                   if        atadtfir = 0
074500091230     c                   z-add     atsdtfic      atadtfir
074600091230     c                   update    aiata000
074700091230     c                   endif
074800091230     c                   enddo
074900091230      * disaccredita  automezzi legati
075000091230     c     kats2         setll     aiatm01l
075100091230     c                   do        *hival
075200091230     c     kats2         reade     aiatm01l
075300091230     c                   if        %eof(aiatm01l)
075400091230     c                   leave
075500091230     c                   endif
075600091230     c                   if        atmdtfir = 0
075700091230     c                   z-add     atsdtfic      atmdtfir
075800091230     c                   update    aiatm000
075900091230     c                   endif
076000091230     c                   enddo
076100091230     C                   ENDSR
076200080912     c**********************************************************************
076300080912     c* carica sfl
076400080912     c**********************************************************************
076500080912     C     Carvideo2     BEGSR
076600080912     C                   IF        WrkCarS01 = *ON
076700080912     C                   EVAL      snrr1   = 0
076800080912     C                   EVAL      *IN90 = *ON
076900080912     C                   EVAL      *IN91 = *OFF
077000080912     C                   WRITE     video2c
077100080912     C                   EVAL      *IN90 = *OFF
077200081016     c     vrags         setll     aiats02l
077300080912     c                   do        *hival
077400081016     c                   read      aiats02l                               97
077500080912     c   97              leave
077600081118      * escludo disaccreditati
077700081118     c                   if        vdis = 'N' and atsdtfic > 0
077800081118     c                   iter
077900081118     c                   endif
078000081118
078100080912     c                   move      *blanks       v1csce
078200081016     c                   move      atsSOC        VI2SOC
078300081016     c                   move      atsKSC        VI2KSC
078400120605     c                   movel     atsRAGS       VI2RAGS
078500120605     c                   move      atsnrc        VI2nrc
078600120605      *data accreditamento
078700081016     c                   if        atsdtinc> 0
078800120605     c                   move      atsdtinc      vi2dtincg
078900081016     c                   move      atsdtinc      dataiso
079000080912     c                   move      dataiso       dataeur
079100081016     c                   move      dataeur       vi2dtinc
079200080912     c                   else
079300081016     c                   clear                   vi2dtinc
079400120605     c                   clear                   vi2dtincg
079500080912     c                   endif
079600120605      *data fine  disaccreditamento
079700081016     c                   if        atsdtfic> 0
079800081016     c                   move      atsdtfic      dataiso
079900081016     c                   move      dataiso       dataeur
080000081016     c                   move      dataeur       vi2dtfic
080100081016     c                   else
080200081016     c                   clear                   vi2dtfic
080300081016     c                   endif
080400120605      *data contratto
080500120605     c                   if        atsdec > 0
080600120605     c                   move      atsdec        dataiso
080700120605     c                   move      dataiso       dataeur
080800120605     c                   move      dataeur       vi2dec
080900120605     c                   else
081000120605     c                   clear                   vi2dec
081100120605     c                   endif
081200080912      *esco per raggiunta massima capacità sfl
081300080912     c                   if        snrr1 > 9990
081400080912     c                   leave
081500080912     c                   endif
081600080912
081700080912     C                   EVAL      snrr1 = snrr1 + 1
081800080912     C                   WRITE     video2
081900080912     C                   ENDDO
082000080912
082100080912     C                   IF        snrr1  > 0
082200080912     C                   EVAL      *IN91 = *ON
082300080912     C                   MOVE      1             OK                1 0
082400080912     C                   ELSE
082500080912     C                   MOVE      0             OK                1 0
082600080912     C                   ENDIF
082700080912     C                   EVAL      nrr1  = 1
082800080912     C                   ENDIF
082900080912     C                   EVAL      WrkCarS01 = *OFF
083000080912     C                   EVAL      snrr1  = 1
083100080912     C                   EVAL      csrrrn = 1
083200080912     C                   ENDSR
083300080912     c**********************************************************************
083400080912     c* gestione sfl
083500080912     c**********************************************************************
083600080912     C     GestioneSFL   BEGSR
083700080912      *
083800080912     C                   EVAL      WrkEofS01 = *OFF
083900080912      * F10 Immissione.
084000081118     C                   IF        *INKJ and visualizza <> *blank
084100080912     c                   clear                   modalita
084200080912     c                   clear                   video3
084300081016     c                   clear                   aiats000
084400081215     c                   seton                                        24
084500120528      *reperisce numero assoluto contratto
084600120528     c                   exsr      repnum
084700120528     c                   move      atsnrc        vatsnrc
084800081031     c                   move      societa       vatssoc
084900080912     c                   move      knmus         vknmus
085000080912     c                   move      knsif         vknsif
085100080912     c                   movel     ragut         rsut
085200090710      **
085300080912     c                   exsr      Gesvideo3
085400090710      **
085500081215     c                   setoff                                       24
085600080912     c                   eval      wrkcars01 = *on
085700080912     c                   if        csrrrn = 0
085800080912     c                   goto      noriga
085900080912     c                   endif
086000080912     c                   endif
086100080912      * Elaborazione opzioni.
086200080912     c*
086300080912     c                   do        *hival        X                 4 0
086400080912     c     X             CHAIN     video2                             50
086500080912     C   50              LEAVE
086600080912     C                   IF        v1csce <> *blanks
086700080912     c                   exsr      opzione
086800080912     c                   move      *blank        v1csce
086900080912     c                   update    video2
087000120608     c   96              leavesr
087100080912
087200080912     C                   ENDIF
087300080912     C                   enddo
087400080912      *
087500090108     c                   if        modalita = '1'
087600090108     c                             or modalita = '2'
087700090108     c                             or modalita = 'C'
087800120606     c                             or modalita = 'N'
087900080912     c                   eval      wrkcars01 = *on
088000080912     c                   endif
088100080912      *
088200080912     C     noriga        ENDSR
088300120528     c**********************************************************************
088400120528     c* reperimento numero contratto
088500120528     c**********************************************************************
088600120528     C     repnum        BEGSR
088700120528     c                   clear                   trul33ds
088800120528     c                   move      'L'           i33tla
088900120528     c                   z-add     37            i33cnu
089000120528     c                   z-add     1             i33num
089100120528     c                   movel     trul33ds      kpjbu
089200120528     c                   call      'TRUL33R'
089300120528     c                   PARM                    kpjba
089400120528     c                   movel     kpjbu         trul33ds
089500120528     c                   z-add     o33nrf        atsnrc
089600120528     C                   ENDSR
089700080912     c**********************************************************************
089800080912     C     *INZSR        BEGSR
089900080912     c**********************************************************************
090000080912      *
090100080912     C     *ENTRY        PLIST
090200080912     C                   PARM                    Kpjba
090300080912     C                   Z-ADD     1             CODUT
090400080912     C                   CALL      'X§PARUT'
090500080912     C                   PARM                    UTEDSE
090600080912     C                   MOVEL     REC80         CNCR80
090700080912     C                   MOVEL     Ragut         rsut
090800080912     C                   MOVEL     knsif         vknsif
090900080912     C                   MOVEL     knmus         vknmus
091000080912      *reperimento data
091100080912     C                   TIME                    W0120            14 0
091200080912     C                   MOVE      W0120         WDAT              8 0
091300080912     C*
091400080912     C                   Z-ADD     WDAT          G02DAT
091500080912     C                   MOVEL     *BLANK        G02ERR
091600080912     C                   CALL      'XSRDA8'
091700080912     C                   PARM                    WLBDA8
091800080912     C* UDATE A 8 IN AAAA/MM/GG
091900080912     C                   Z-ADD     G02INV        WUDA              8 0
092000080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
092100080912     C                   CLEAR                   Tibs36Ds
092200080912     C                   EVAL      I36ute = knmus
092300080912     C                   CALL      'TIBS36R'
092400080912     C                   PARM                    tibs36ds
092500081118      * profilo di filiale
092600081118     c                   if        o36pou <> 046
092700081118     c                   seton                                        30
092800081118     c                   endif
092900080912
093000081118      * azione di visualizzazione
093100081118     c                   if        kcdaz = 'TL82'
093200081118     c                   move      'X'           visualizza        1
093300081118     c                   endif
093400081118
093500081016     C     Kfrn          KLIST
093600081016     C                   KFLD                    societa           3
093700081016     C                   KFLD                    codice            8
093800080912      *
093900081016     C     Kats          KLIST
094000081016     C                   KFLD                    vi2soc
094100081016     C                   KFLD                    vi2ksc
094200120605      *
094300120605     C     Katsd         KLIST
094400120605     C                   KFLD                    vi2soc
094500120605     C                   KFLD                    vi2ksc
094600120605     C                   KFLD                    vi2dtincg
094700120605     C                   KFLD                    vi2nrc
094800091230      *
094900091230     C     Kats2         KLIST
095000091230     C                   KFLD                    atssoc
095100091230     C                   KFLD                    atsksc
095200090713     C*
095300090713     c     Ktab          klist
095400090713     c                   kfld                    tblkut
095500090713     c                   kfld                    tblcod
095600090713     c                   kfld                    tblkey
095700090713     c                   z-add     1             tblkut
095800090713     C*
095900100108     c                   move      '3I'          tblcod
096000100108     c                   movel(p)  '1'           tblkey
096100100108     c     ktab          chain     tabel00f
096200100108     c                   if        %found(tabel00f)
096300100108     c                   movel     tbluni        ds3i
096400100108     c                   eval      societa = %subst(§3IBST: 7: 3)
096500100104     c                   move      societa       vi2soc
096600100104     c                   else
096700120608     c                   z-add     219           vi2soc
096800120608     c                   move      219           societa
096900100104     c                   endif
097000080912      *---------------------------------------------------------------------
097100080912     C                   ENDSR
