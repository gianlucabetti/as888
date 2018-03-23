000100090410     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200121204     H BNDDIR('TIBS') EXTBININT(*YES)
000300000000      *****************************************************************
000400000000      *
000500090410      *  Descrizione   :  Ristampa contratti autotrasp.
000600000000      *****************************************************************
000700020307     Fficn15D   CF   E             WORKSTN
000800090410     ffiapd01l  if   e           k disk
000900160406     ftntbe01l  if   e           k disk
001000090521     faitra01l  if   e           k disk
001100090410     faitrs01l  uf   e           k disk    commit
001200090521     ffitgt00f  uf   e             disk    commit
001300090521     ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
001400090521     F                                     INFDS(tgtDS)
001500090521     ffiadt00f  uf   e             disk    commit
001600090521     ffiadt01l  if   e           k disk    rename(fiadt000:fiadt001)
001700090521     F                                     INFDS(adtDS)
001800000000      *
001900090521     D TgtDS           DS
002000090521     D  tgtNRR               397    400B 0
002100090521     D adtDS           DS
002200090521     D  adtNRR               397    400B 0
002300090521     d savnrtgt        s                   like(tgtnrr)
002400090521     d savnradt        s                   like(adtnrr)
002500090521     d savprg          s                   like(adtprg)
002600000000     D KPJBA         E DS
002700000000     D CNCR80        E DS
002800090519     D trmz70s1ds    E DS
002900090519     D trmzctrds     E DS
003000090521     D dtgtflr       E DS
003100090521     D dtrsflr       E DS
003200130115     D dtrsflr1      E DS
003300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003400090410     D WLBDA8          DS
003500090410     D  G08DAT                 1      8  0
003600090410     D  G08INV                 9     16  0
003700090410     D  G08ERR                17     17
003800090410     D  G08TGI                18     22  0
003900940926     D*
004000000000     D WLBDAT          DS
004100940926     D  G02DAT                 1      8  0
004200940926     D  G02INV                 9     16  0
004300940926     D  G02ERR                17     17
004400940926     D  G02TGI                18     22  0
004500090519     D trmz0100i     E DS                  prefix(x)
004600090519     d dataiso         s               d
004700160406     d datapdf         s              8  0
004800090521     d param           ds
004900090521     d   partip                       1
005000090521     d   parpdrda                     7  0
005100090521     d   parpdra                      7  0
005200090521     d   pardat                       8  0
005300090521     d   pardtcert                    8  0
005400090521     d   parnrc                       7s 0
005500090521     d   parfil                       3s 0
005600090521     D   paroutq                     10
005700090521     d   parerr                       1
005800121203     D   msgError                    35
005900121203     D   pgmChiamante                10
006000111205     d   pardfa                       8  0
006100121204     d   parsocprt                    3
006200090521     d paramdef        ds
006300090521     d   parpdrd                      7  0
006400090521     d   parprgd                      3  0
006500090521     d   partipd                      1
006600090521     d   pardtcertd                   8  0
006700090521     d   parnrcd                      7s 0
006800090521     d   parfild                      3s 0
006900090521     D   paroutqd                    10
007000090521     d   parerrd                      1
007100111205     d   pardfad                      8  0
007200121204     d   parsocprtd                   3
007300120222
007400090519     D/COPY GAITRASRC/SRCPROTOPR,TRMZ0100R
007500090519     D prmRpyOpCode...
007600090519     D                 S             10A
007700090519     D prmRpyIdMsg...
007800090519     D                 S             10I 0
007900121204ab    **
008000121204ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
008100121204ab    /DEFINE DFTACTGRP_NO
008200121204ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
008300121204ab    /UNDEFINE DFTACTGRP_NO
008400121204      *
008500121204ab   D TibsSof_esito...
008600121204ab   D                 S             10I 0 IMPORT
008700121204ab   D idSocieta...
008800121204EDPDCD                 S              3A
008900121204ab   **
009000090410      *--------------------------------------------------------------*
009100090410      *             M A I N      L I N E
009200090410      *--------------------------------------------------------------*
009300121204ab    /FREE
009400121204ab     TibsSof_Init();
009500121204ab    /END-FREE
009600000000      * Inizializzo i campi a video
009700090518     c                   exsr      inz01
009800000000      * Emetto il formato video
009900090410     C                   do        *hival
010000020308     C                   SETOFF                                       90
010100020307     C                   EXFMT     cn15D01
010200000000      *
010300090410     C   KC              leave                                                  F3=USCITA
010400000000      * Eseguo i  controlli sui campi immessi a video
010500000000     C                   EXSR      CONTR
010600090410     c   90              iter
010700020307      * Richiamo programma di stampa
010800020308     c                   if        *inkf
010900090410     c                   exsr      srstampa
011000090520     c   90              iter
011100090519     c                   exsr      sraggio
011200090522     c                   commit
011300090717     c                   exsr      inz01
011400020308     c                   endif
011500000000      *
011600090410     C                   enddo
011700090519      /FREE
011800090519       trmz0100r( 'FINALIZE'
011900090519                : prmRpyOpCode
012000090519                : prmRpyIdMsg
012100090519                );
012200090519
012300090519      /END-FREE
012400000000     C                   SETON                                        LR
012500090521      * ?___________________________________________________________________
012600000000      * Routine per i controlli sui campi del formato video          *
012700090521      * ?___________________________________________________________________
012800090518     C     inz01         BEGSR
012900090518     c                   setoff                                       90
013000090518     C                   eval      v1ctipris ='E'                               DATA INIZIALE
013100090521     C                   eval      V1ctipo = 'C'                                DATA FINALE
013200090518     C                   clear                   v1cnrc                         1' COD.PADRONC.
013300090518     C                   clear                   v1cpdr                         1' COD.PADRONC.
013400090518     C                   clear                   v1crsf                         1' COD.PADRONC.
013500090518     C                   clear                   v1cddt                         1' COD.PADRONC.
013600090522     C                   movel     GMAUDt        v1cdst                         1' COD.PADRONC.
013700090518     c                   endsr
013800090521      * ?___________________________________________________________________
013900090518      * Routine per i controlli sui campi del formato video          *
014000090521      * ?___________________________________________________________________
014100090518     C     CONTR         BEGSR
014200090519     c                   setoff                                       90
014300090519     c                   clear                   msgv
014400090410     c                   do
014500090527     c* data decorrenza tariffa se richiesta ristampa allegato
014600090527     c                   if        v1ctipris <>'C'
014700090527     c                             and v1cddt = 0
014800090527     c                   seton                                        0790
014900090527     C                   leave
015000090527     c                   end
015100090527     C     V1cddt        IFGT      0
015200090527     C                   Z-ADD     V1cddt        G02DAT
015300090527     C                   MOVEL     *BLANK        G02ERR
015400090527     C                   CALL      'XSRDA8'
015500090527     C                   PARM                    WLBDAT
015600090527     C     G02ERR        IFEQ      '1'
015700090527     C                   SETON                                        07  90
015800090527     C                   leave
015900090527     C                   ENDIF
016000090527     C                   Z-ADD     G02INV        comddt            8 0
016100090527     C                   Z-ADD     G02dat        v1cddt
016200090527     C                   ENDIF
016300120222      *
016400090518      * Controlli numero contratto obbligatorio
016500090410     C                   IF        v1cnrc = *zero
016600090410     c                   seton                                        0290
016700090410     C                   leave
016800120222      *
016900120222      ** se il numero contratto è quello di PEDINI ossia è un trazionista
017000120222      *   che fa anche affluenze e defluenze.
017100120607     c*************      elseif    v1cnrc = 9999999
017200120607     c                   elseif    v1cnrc >= 9000000
017300120222      *
017400120222     c* errore se si vuole stampare anche il contratto con il trazionista
017500120222     c****  è solo possibile stampare l'allegato tariffario
017600120222     c                   if        v1ctipris <> 'A'
017700120222     C                   SETON                                        1090
017800120222     C                   leave
017900120222     C                   ENDIF
018000120222      *
018100120222     c*   deve essere possibile solo per tariffe di affluenze e defluenze
018200120222     c                   if        v1ctipo <> 'A'
018300120222     C                   SETON                                        1190
018400120222     C                   leave
018500120222     C                   ENDIF
018600120222      *
018700120222      **  se invece è tutt'altro
018800120222     c                   else
018900120222      **
019000090521     C     v1cnrc        chain(n)  aitrs01l
019100090410     c                   if        not %found(aitrs01l) or
019200090521     c                             (%found(aitrs01l) and trsann <> ' ')
019300090410     c                   seton                                        0290
019400090410     C                   leave
019500090410     c                   else
019600090410     c* errore se contratto non ancora stampato
019700090522     c                   if        trsdsc = 0 and v1ctipris <> 'A'
019800090410     C                   SETON                                        9004
019900090410     C                   leave
020000090410     C                   ENDIF
020100090410     c* errore se presente data ricezione copia firmata
020200090410     c                   if        trsdrc <> 0 and v1ctipris <> 'A'
020300090410     C                   SETON                                        9004
020400090410     C                   leave
020500090410     C                   ENDIF
020600160407     c* errore se data stampa maggiore di data partenza pdf
020700160407     c                   if        trsdsc <> 0 and
020800160407     c                             trsdsc > datapdf
020900160406     C                   SETON                                        9011
021000160406     C                   leave
021100160406     C                   ENDIF
021200120222      *
021300090410     C                   ENDIF
021400120925      *  essendo ancora dentro al contratto NON trazionista
021500120925      *   cerca su AITRA (la end è spostata più avanti)
021600120925     C**********         ENDIF
021700090410     c* controllo che sia presente almeno un autista accreditato
021800090521     c                   clear                   ok                1
021900090521     C     v1cnrc        setll     aitra01l
022000090410     c                   do        *hival
022100121204     C     v1cnrc        reade     aitra01l
022200090410     c                   if        %eof(aitra01l)
022300090410     c                   leave
022400090410     c                   end
022500090518     c                   if        v1cpdr <> 0
022600090518     c                   if        trakau <> v1cpdr
022700090518     c                             and v1ctipo = 'C'
022800090518     c                   iter
022900090518     c                   end
023000090518     c                   if        trabpt <> v1cpdr
023100090518     c                             and v1ctipo = 'A'
023200090518     c                   iter
023300090518     c                   end
023400090518     c                   end
023500090521     c                   if        traann <> ' '
023600090410     c                   iter
023700090410     c                   end
023800090410     c                   movel     '1'           ok
023900090410     c                   leave
024000091013     c*                  end
024100090410     C                   ENDdo
024200090410     c*
024300090410     c                   if        ok = ' '
024400090410     C                   SETON                                        9005
024500090410     C                   leave
024600090410     c                   end
024700120925     C                   ENDIF
024800090522     c* controllo se immesso un codice aut
024900090522     c                   if        v1ctipris <> 'C'
025000090522     c                   clear                   savnrtgt
025100090522     c                   if        v1cpdr = 0
025200090522     C                   SETON                                        9009
025300090522     C                   leave
025400090522     c                   else
025500090522     c                   if        v1ctipo = 'C'
025600090522     c                   eval      apdtip = 'A'
025700090522     c                   else
025800090522     c                   eval      apdtip = 'D'
025900090522     c                   end
026000090522     c     kapd          chain     fiapd01l
026100090522     c                   if        %found(fiapd01l)
026200091102     c********                     and apdatb = ' '
026300090522     c                   movel     apdrsf        v1crsf
026400090522     c                   else
026500090522     C                   SETON                                        9009
026600090522     C                   leave
026700090522     c                   end
026800090522     c                   end
026900090522     c                   end
027000120222      *
027100120222      *   se sto ristampando x affl/defl e con contratto di Pedini
027200090522     c* data certa
027300090522     C     V1cdst        IFeq      0
027400090522     C                   SETON                                        08  90
027500090522     C                   leave
027600090522     c                   else
027700090522     C                   Z-ADD     V1cdst        G02DAT
027800090522     C                   MOVEL     *BLANK        G02ERR
027900090522     C                   CALL      'XSRDA8'
028000090522     C                   PARM                    WLBDAT
028100090522     C     G02ERR        IFEQ      '1'
028200090522     C                   SETON                                        08  90
028300090522     C                   leave
028400090522     C                   ENDIF
028500090522     C                   Z-ADD     G02INV        comdst            8 0
028600090522     C                   Z-ADD     G02dat        v1cdst
028700090522     C                   ENDIF
028800090522      * data certa minore di udate non possibile
028900090522     c                   if        comdst < oggi
029000090522     C                   SETON                                        0890
029100090522     c                   leavesr
029200090522     c                   endif
029300090522      * data certa errore se maggiore di udate + 7 giorni
029400090522     C                   if        comdst > oggi7
029500090522     C                   SETON                                        0890
029600090522     c                   leavesr
029700090522     c                   endif
029800090410     c* controllo che sia presente almeno un allegato da stampare
029900090522     c                   if        v1ctipris <> 'C'
030000090410     c                   clear                   ok
030100090519     c* aut
030200090521     c                   if        v1ctipo = 'C'
030300090410     C     ktgt          setll     fitgt01l
030400090410     c                   do        *hival
030500090410     C     ktgt          reade     fitgt01l
030600090410     c                   if        %eof(fitgt01l)
030700090410     c                   leave
030800090410     c                   end
030900090410     c                   if        tgtatb <> ' '
031000090410     c                   iter
031100090410     c                   end
031200090518     c                   if        tgtddt <> comddt
031300090518     c                   iter
031400090518     c                   end
031500090521     c* verifico codice aut
031600090521     c                   if        tgtpdr <> v1cpdr
031700090410     C                   iter
031800090410     C                   ENDIF
031900090410     c                   if        tgtdcn <>0 and tgtdts <> 0 and
032000160608     c*                            tgtdrc = 0 and tgtnrc = v1cnrc
032001160608     c                             tgtnrc = v1cnrc
032100160608     c*                            and tgtdcn < datapdf
032200090522     c                   eval      savnrtgt = tgtnrr
032300090410     c                   movel     '1'           ok
032400090410     c                   leave
032500090410     c                   end
032600090410     C                   ENDdo
032700090519     c* non trovato l'allegato da stampare
032800090518     c                   if        ok = ' '
032900160406     c                   if        tgtdcn >= datapdf
033000160406     C                   SETON                                        9012
033100160406     C                   leave
033200160407     c                   else
033300090518     C                   SETON                                        9006
033400090518     C                   leave
033500160406     c                   end
033600090518     c                   end
033700090519     c                   end
033800090410     c* aff/defl
033900090521     c                   if        v1ctipo = 'A'
034000090521     c                   clear                   savnrtgt
034100090521     c                   clear                   savprg
034200090521     C     v1cpdr        setll     fiadt01l
034300090410     c                   do        *hival
034400090521     C     v1cpdr        reade     fiadt01l
034500090410     c                   if        %eof(fiadt01l)
034600090410     c                   leave
034700090410     c                   end
034800090410     c                   if        adtatb <> ' '
034900090410     c                   iter
035000090410     c                   end
035100090518     c                   if        adtddt <> comddt
035200090518     c                   iter
035300090518     c                   end
035400090410     c* se richiesto un codice verifico solo quello
035500090521     c                   if        adtpdr <> v1cpdr
035600090410     C                   iter
035700090410     C                   ENDIF
035800090410     c                   if        adtdcn <>0 and adtdts <> 0 and
035900090410     c                             adtdrc = 0 and adtnrc = v1cnrc
036000160406     c                             and adtdcn < datapdf
036100090522     c                   eval      savnradt = adtnrr
036200090522     c                   eval      savprg = adtprg
036300090410     c                   movel     '1'           ok
036400090410     c                   leave
036500090410     c                   end
036600090410     C                   ENDdo
036700090410     c*
036800090410     c                   if        ok = ' '
036900160406     c                   if        adtdcn >= datapdf
037000160406     C                   SETON                                        9012
037100160406     C                   leave
037200160406     c                   else
037300090518     C                   SETON                                        9006
037400090410     C                   leave
037500160406     c                   end
037600090410     c                   end
037700090519     c                   end
037800090518     c                   end
037900090519     c* chaimo il controllore
038000120925      ***
038100120925      ***  Se contratto trazionista :
038200120925     c                   if        v1cnrc >= 9000000
038300120925      ***
038400120925     c                   else
038500120925      ***
038600120925      ***  se NON trazionista :
038700090519     c                   clear                   trmzctrds
038800090519     c                   eval      dctrnrc = v1cnrc
038900090521     c                   if        v1cpdr <> 0
039000090522     c                   eval      dctrtipo = v1ctipo
039100090519     c                   eval      dctrpdr = v1cpdr
039200090519     c                   end
039300090519     c                   if        v1ctipris <> 'C'
039400090519     c                   eval      dctrddt = comddt
039500090519     c                   end
039600090519     c                   movel     trmzctrds     kpjbu
039700090519     c                   call      'TRMZCTRR'
039800090519     c                   parm                    kpjba
039900090519     c                   movel     kpjbu         trmzctrds
040000090519     c                   if        dctrerr <> ' '
040100090519     c                   movel     dctrmsg       msgv
040200090519     c                   seton                                        90
040300090519     c                   end
040400120925      ***
040500120925     c                   end
040600120925      ***
040700090410     c                   enddo
040800000000      *
040900090410     C                   ENDSR
041000090521      * ?___________________________________________________________________
041100090519      * Routine di stmpa
041200090521      * ?___________________________________________________________________
041300090519     C     srstampa      BEGSR
041400090521     c* contratto
041500090519     c                   if        v1ctipris <> 'A'
041600090522     c* alloco prima di stampare il record
041700090522     C     v1cnrc        chain(e)  aitrs01l
041800090522     c                   if        %error
041900090522     c                   seton                                        90
042000090522     c                   eval      msgv = 'Contratto allocato da altro lavoro'
042100090522     c                   leavesr
042200090522     c                   end
042300111206     c                   eval      dtrsflr = trsflr
042400090519     c                   exsr      contratto
042500090520     c   90              leavesr
042600090519     c                   end
042700090521     c* allegato
042800090520     c                   if        v1ctipris <> 'C'
042900090521     c                   if        v1ctipo = 'C'
043000090522     C     savnrtgt      chain(e)  fitgt00f
043100090522     c                   if        %error
043200090522     c                   seton                                        90
043300090522     c                   eval      msgv = 'Tariffa allocato da altro lavoro'
043400090522     c                   leavesr
043500090522     c                   end
043600111205     c                   eval      dtgtflr = tgtflr
043700090521     c                   exsr      allegatocit
043800090521     c                   else
043900090522     C     savnradt      chain(e)  fiadt00f
044000090522     c                   if        %error
044100090522     c                   seton                                        90
044200090522     c                   eval      msgv = 'Tariffa allocato da altro lavoro'
044300090522     c                   leavesr
044400090522     c                   end
044500090521     c                   exsr      allegatoad
044600090521     c                   end
044700090520     c   90              leavesr
044800090520     c                   end
044900090519     C                   ENDSR
045000090521      * ?___________________________________________________________________
045100090519      * Routine per i controlli sui campi del formato video          *
045200090521      * ?___________________________________________________________________
045300090519     C     sraggio       BEGSR
045400090521     c* aggiorno data di stampa e flag di ristampato contratto
045500090518     c                   if        v1ctipris <> 'A'
045600090521     C     v1cnrc        chain     aitrs01l
045700090522     c                   if        %found(aitrs01l) and trsann = ' '
045800090521     c                   movel     comdst        trsdsc
045900111206     c                   eval      dtrsflr = trsflr
046000090521     c                   movel     'R'           §trsris
046100090518     c                   movel     dtrsflr       trsflr
046200160608     c*                  update    aitrs000
046300090518     c                   end
046400090518     c                   end
046500090521     c* aggiorno data di stampa e flag di ristampato allegato
046600090521     c                   if        v1ctipris <> 'C'
046700090521     c                   if        v1ctipo = 'C'
046800090521     C     savnrtgt      chain     fitgt00f
046900090521     c                   if        %found(fitgt00f)
047000090521     c                   movel     comdst        tgtdcn
047100111206     c                   eval      dtgtflr = tgtflr
047200090521     c                   movel     'R'           §tgtris
047300090521     c                   movel     dtgtflr       tgtflr
047400160608     c*                  update    fitgt000
047500090521     c                   end
047600090522     c                   end
047700090521     c                   if        v1ctipo = 'A'
047800090521     C     savnradt      chain     fiadt00f
047900090521     c                   if        %found(fiadt00f)
048000090521     c                   movel     comdst        adtdcn
048100090522     c                   movel     'R'           adttsr
048200160608     c*                  update    fiadt000
048300090521     c                   end
048400090521     c                   end
048500090522     c                   end
048600090410     C                   ENDSR
048700090519      * ?___________________________________________________________________
048800090519     c     Contratto     begsr
048900090519      * ?___________________________________________________________________
049000090522      /FREE
049100090522       trmz0100r( 'INIT'
049200090522                : prmRpyOpCode
049300090522                : prmRpyIdMsg
049400090522                );
049500090522       IF prmRpyIdMsg < 0;
049600090522         // Stampa non riuscita.
049700090522       ENDIF;
049800090522      /END-FREE
049900090519     c* Richiamo PGM per stampa del contratto
050000090519     c                   exsr      RagSocCont
050100090520     c                   if        not *in90
050200090519     c                   clear                   prmRpyOpCode
050300090519     c                   clear                   prmRpyIdMsg
050400090519     c                   clear                   trmz0100i
050500090519     c                   move      trsdec        dataiso
050600090519     c                   eval      xCONTRATDAT  = dataiso
050700090519     c                   eval      xCONTRATNUM  = trsnrc
050800090519     c                   eval      xFILIALE     = trsfil
050900090519     c                   eval      xVETRAGSOC   = %trim(ragsocksc) + ' ' +
051000090519     c                                          %trim(ragsocksc1)
051100090519     c                   eval      xVETSLCOM    = %trim(localita)
051200090519     c                   eval      xVETSLIND    = %trim(indirizzo)
051300090519     c                   eval      xVETSLCAP    = %trim(cap)
051400090519     c                   eval      xVETSLPRV    = provincia
051500090519     c                   eval      xVETSLNAZ    = stato
051600090519     c                   eval      xVETCDFISCA  = CodiceFisc
051700090519     c                   eval      xVETCDIVA    = trsiva
051800090519     c                   eval      xVETRIMLUO   = %trim(trscit)
051900090519     c                   eval      xVETRIMNUM   = trsnis
052000090519     c                   eval      xVETLEGRAP   = %trim(trslra)
052100090519     c                   eval      xVETANAPRV   = trspia
052200090519     c                   eval      xVETANANUM   = trsnia
052300090519     c                   move      comdst        dataiso
052400090519     c                   eval      xdatastampa = dataiso
052500160518     c                   eval      xdatacertap = dataiso
052600160518      * per ristampa non passo più la data di stampa precedente per la versione perchè
052700160518      * altrimenti viene errata il reperimento delle OVR e non genera il PDF
052800160518     c*                  if        §trsunodc <>' '
052900160518     c*                  move      §trsunodc     data              8 0
053000160518     c*                  move      data          xdatacertap
053100160518     c*                  end
053200130115      * Reperisce società da stampare sul contratto x la RISTAMPA
053300130115     c                   eval      dtrsflr1 = trsflr1
053400130115     c                   eval      xSOCIETA     = §TRSSOCPRT
053500130115      ***
053600111206     c                   if        §trsdfa <>' '
053700111206     c                   move      §trsdfa       data              8 0
053800111206     c                   move      data          xdatafirmav
053900111206     c                   end
054000090522     c                   endif
054100090519      /FREE
054200090519       trmz0100r( 'PRTCONTRAT'
054300090519                : prmRpyOpCode
054400090519                : prmRpyIdMsg
054500090519                : 'TRMZ0100I'
054600090519                : trmz0100i
054700090519                : %SIZE(trmz0100i)
054800090519                );
054900090519       IF prmRpyIdMsg < 0;
055000090519         // Stampa non riuscita.
055100090519       ENDIF;
055200090522      /END-FREE
055300090522      * c'è errore stampo la segnalazione
055400090522     c                   if        prmrpyidmsg <>0
055500090522     c                   eval      msgv ='Errore nella stampa del contratto'
055600090522     c                   seton                                        90
055700090522     c                   endif
055800090519     c                   endsr
055900090519      * ?___________________________________________________________________
056000090519     c     AllegatoCit   begsr
056100090519      * ?___________________________________________________________________
056200090519     c* Richiamo PGM per stampa allegato autista di città
056300090519     c                   clear                   param
056400090521     c                   z-add     v1cpdr        parpdrda
056500090521     c                   z-add     v1cpdr        parpdra
056600090521     c                   z-add     comddt        pardat
056700090519     c                   move      'C'           partip
056800090521     c                   z-add     comdst        pardtcert
056900090519     c                   z-add     trsfil        parfil
057000090519     c                   z-add     trsnrc        parnrc
057100111205     c                   if        §tgtdfa <>' '
057200111205     c                   move      §TGTDFA       pardfa
057300111205     c                   end
057400130115      ***
057500130115      * Reperisce società da stampare sul contratto x la RISTAMPA
057600130115     c                   eval      dtrsflr1 = trsflr1
057700130115     c                   eval      parsocprt    = §TRSSOCPRT
057800121204      *
057900090519     c                   movel     param         kpjbu
058000090519     c                   call      'FICN14R'
058100090519     c                   parm                    kpjba
058200090519     c                   movel     kpjbu         param
058300090521     c                   if        parerr <>*blank
058400090521     c                   seton                                        90
058500090521     c                   eval      msgv ='Errore nella stampa ALLEGATO '
058600090519     c                   endif
058700090521     c                   endsr
058800090519      * ?___________________________________________________________________
058900090519     c     AllegatoAd    begsr
059000090519      * ?___________________________________________________________________
059100090519     c* Richiamo PGM per stampa allegato autista di AFF/DEF
059200090519     c                   clear                   paramdef
059300090521     c                   z-add     v1cpdr        parpdrd
059400090521     c                   z-add     savprg        parprgd
059500090519     c                   move      'C'           partipd
059600090521     c                   z-add     comdst        pardtcertd
059700130311     c                   if        v1cnrc < 9000000
059800090519     c                   z-add     trsfil        parfild
059900090519     c                   z-add     trsnrc        parnrcd
060000130311      * Reperisce società da stampare sul contratto x la RISTAMPA
060100130311     c                   eval      dtrsflr1 = trsflr1
060200130311     c                   eval      parsocprtd   = §TRSSOCPRT
060300130311     c                   else
060400130311     c                   z-add     adtfil        parfild
060500130311     c                   z-add     adtnrc        parnrcd
060600130311     c                   eval      parsocprtd   = adtcsf
060700130311     c                   endif
060800111205     c                   eval      pardfad = adtdfa
060900130115      ***
061000121204      *
061100090519     c                   movel     paramdef      kpjbu
061200090519     c                   call      'FICNA6C'
061300090519     c                   parm                    kpjba
061400090519     c                   movel     kpjbu         paramdef
061500090521     c                   if        parerrd <>*blank
061600090519     c                   select
061700090519     c                   when      parerrd = '1'
061800090521     c                   eval      msgv ='Testata tariffa annullata '
061900090519     c                   when      parerrd = '2'
062000090521     c                   eval      msgv ='Testata tariffa non trovata '
062100090519     c                   when      parerrd = '3'
062200090521     c                   eval      msgv ='Anagrafica autista annullata '
062300090519     c                   endsl
062400090521     c                   seton                                        90
062500090519     c                   endif
062600090519     c                   endsr
062700090521      * ?___________________________________________________________________
062800090519     C     RagSocCont    BegSR
062900090519      * ?___________________________________________________________________
063000090519     C                   clear                   Trmz70s1ds                     Output
063100090519     C                   movel(p)  TRSiva        PartitaIVA                     Input
063200090519     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
063300090519     C                   movel(p)  TRSFIL        Unita                          Input/Output
063400090608     C                   movel(p)  TRSsocg       societa                        Input/Output
063500090519      *
063600090519     c                   call      'TRMZ70SR1'
063700090519     C                   PARM                    trmz70s1ds
063800090519     c                   if        errore <> '0'
063900090519     c                   seton                                        90
064000090519     c                   eval      msgv = errmsg
064100090519     c                   endif
064200090519     c*
064300090519     C                   ENDSR
064400090521      * ?___________________________________________________________________
064500090521     c     *inzsr        begsr
064600090521      * ?___________________________________________________________________
064700090521     C     *ENTRY        PLIST
064800090521     C                   PARM                    KPJBA
064900090521      *
065000090521     C                   Z-ADD     1             CODUT
065100090521     C                   CALL      'X§PARUT'
065200090521     C                   PARM                    UT§DSE
065300090521     C                   MOVEL     RAGUT         RSUT             20
065400090521     C                   MOVEL     RAGUT         RSUT2            20
065500090521     C                   MOVEL     REC80         CNCR80
065600090521     C                   TIME                    WHHUDT           14 0
065700090521     C                   TIME                    TIME              6 0
065800090521     C                   MOVE      WHHUDT        G08DAT
065900090521     C                   MOVE      *ZEROS        G08INV
066000090521     C                   MOVEL     *BLANK        G08ERR
066100090521     C                   CALL      'XSRDA8'
066200090521     C                   PARM                    WLBDA8
066300090521     C                   Z-ADD     G08INV        oggi              8 0
066400090521     C                   Z-ADD     G08DAT        GMAUDt            8 0
066500090521     c*
066600090522     c                   movel     GMAUDt        v1cdst
066700090522     c                   movel     oggi          dataiso
066800090522     c     dataiso       adddur    7:*d          dataiso
066900090522     c                   movel     dataiso       oggi7             8 0
067000090521     c*
067100090521     c     kapd          klist
067200090522     c                   kfld                    apdtip
067300090521     c                   kfld                    v1cpdr
067400090521     c     ktgt          klist
067500090521     c                   kfld                    v1cpdr
067600090521     c                   kfld                    comsml            1
067700160406      *
067800160406     c     ktbe          klist
067900160406     c                   kfld                    tbecod
068000160406     c                   kfld                    tbeke1
068100160406     c                   z-add     20391231      datapdf
068200160406      *reperisco limiti da tabella per calcolare il D-Day
068300160406     c                   movel(p)  '*PDF'        tbeke1
068400160406     c                   movel     '007'         tbecod
068500160406     c     ktbe          chain     tntbe01l                           90
068600160406     c                   if        not *in90
068700160406     c                   movel     tbeKE2        dataiso
068800160406     c                   move      dataiso       datapdf
068900160406     c                   endif
069000120222     C*
069100090521     c                   endsr
069200120222     C**************************************************************************
