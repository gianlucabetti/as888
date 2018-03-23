000100160819     H*------------------------------------------------------------------------*
000200160819     H* PGM schedulato per flaggare le bolle la cui merce deve essere          *
000300160819     H*  portata nel magazzino giacenze
000400160819     H*------------------------------------------------------------------------*
000500990504     H* ACTGRP=*CALLER
000600990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000700160819     H*------------------------------------------------------------------------*
000800160822     ftigcp26l  uf   e           k disk
000900160822     FFNevb01L  IF   E           K DISK
001000160822     FTABEL00F  IF   E           K DISK    usropn
001100160822     Ftntbe01l  uF a E           K DISK
001200160822     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300160822
001400050221     D FIL£6           S              3  0 DIM(30)
001500160822     D codntc          S              3    DIM(250)
001600160822
001700050221     D WLBDA8          DS
001800050221     D  G08DAT                 1      8  0
001900050221     D  G08INV                 9     16  0
002000050221     D  G08ERR                17     17
002100050221     D  G08TGI                18     22  0
002200050221      *
002300070629
002400050221     D CNCR80        E DS
002500050221     D KPJBA         E DS
002600160822     D ds2a          E DS
002700160822     D DSDF          E DS
002800160822     d
002900160822     D FNLGC4DS      E DS
003000160822     d
003100160822     D WDAT8           DS
003200160822     D  DATADA                 1      8  0
003300160822     D  DATAA                  9     16  0
003400160822     D  GGL                   17     21  0
003500160822     D tfptfa          DS
003600160822     D  p_tfp                  1      3  0
003700160822     D  p_tfa                  4      6  0
003800160822     d
003900050221     d trul06ds      e ds
004000050221     d  lin                    1     90  0 dim(30)
004100050221      *
004200160822     d kTbeCod         s                   Like(TbeCod)
004300160822     d kTbeKe1         s                   Like(TbeKe1)
004400160824     d Datasys         s               d   datfmt(*iso) inz(*sys)
004500070629      *----------------------------------------------------------------
004600160822     C     *ENTRY        PLIST
004700160822     C                   PARM                    KPJBA
004800160822     C                   MOVEL     KPJBU         fnlgc4ds
004900160822     c
005000160822     c                   exsr      Car£6
005100160819     c
005200160822    0C                   DO        30            I                 2 0
005300160822    1c                   if        fil£6(i) = 0
005400050221     c                   leave
005500160822    1c                   endif
005600160822
005700050221     C                   Z-ADD     fil£6(i)      FLE               3 0
005800050221     C*
005900160819     c   01              z-add     10            kfas
006000050221      * lettura giacenze
006100050418     c     kgcp          setll     tigcp26l
006200160822    1c                   do        *hival
006300050418     c     fle           reade     tigcp26l                             90
006400050413      *allocazione
006500160822    2c                   if        *in90
006600050418     c     fle           reade(n)  tigcp26l
006700160822    3c                   if        %eof
006800050413     c                   leave
006900160822    3c                   end
007000050413     c                   iter
007100160822    2c                   endif
007200050413      *
007300160822    2c                   if        %eof
007400000607     c                   leave
007500160822    2c                   end
007600160822     c
007700160822     c                   clear                   waggior           1
007800160822     c                   clear                   weve_tc           1
007900160822
008000160822      * la fase non può essere > 26
008100160822    2c                   if        gcpfas > 30
008200050221     c                   leave
008300160822    2c                   end
008400050221
008500050408      *solo progressivo 0
008600160822    2c                   if        gcpfrg <> 0
008700050408     c                   iter
008800160822    2c                   end
008900160822
009000160822      * escludo se  già aggiornata
009100160822    2c                   if        gcpDLMA>0
009200160822     c                   iter
009300160822    2c                   end
009400160822
009500160822      * Se riapertura --> aggiorno subito
009600160822    2c                   if        GCPRIAP='S'
009700160822     c                   eval      waggior='S'
009800160822    2c                   endif
009900160822
010000160822     c* se non è presente un evento di mancata consegna --> aggiorno subito
010100160822    2c                   if        waggior=' '
010200160822     c     karb          setgt     fnevb01l
010300160822     c     karb          readpe    fnevb01l
010400160822    3c                   dow       not %eof(fnevb01l)
010500160822
010600160822    4c                   if        %lookup(evbcev:codntc)>0
010700160822     c* trovato evento di mancata consegna --> verifico la data rispetto
010800160822     c*  alla data giacenza
010900160822     c                   eval      weve_tc='S'
011000160822     c* Verifico i giorni lavorativi tra la data evento e la data
011100160822     c*  apertura giacenza
011200160822     c                   clear                   wdat8
011300160822     c                   clear                   tfptfa
011400160822     c                   z-add     gcpmgc        datada
011500160822     c                   movel     gcpagc        datada
011600160822     c                   z-add     evbdev        dataa
011700160822     c                   z-add     ILGC4LNA      p_tfa
011800160822     c                   call      'XSRLAV8'
011900160822     c                   parm                    wdat8
012000160822     c                   parm                    tfptfa
012100160822     c
012200160822     c* se i giorni lavorativi sono più di due --> aggiorno la data
012300160824     c*  tra la data evento e la data apertura giacenza
012400160822    5c                   if        GGL > 2
012500160822     c                   eval      waggior='S'
012600160824   x5c                   else
012700160824     c* altrimenti  passati due giorni dall'apertura giacenza
012800160824     c*  i colli passano in IMG
012900160824     c                   clear                   wdat8
013000160824     c                   clear                   tfptfa
013100160824     c                   z-add     gcpmgc        datada
013200160824     c                   movel     gcpagc        datada
013300160824     c                   z-add     dateu         dataa
013400160824     c                   z-add     ILGC4LNA      p_tfa
013500160824     c                   call      'XSRLAV8'
013600160824     c                   parm                    wdat8
013700160824     c                   parm                    tfptfa
013800160824
013900160824    6c                   if        GGL > 2
014000160824     c                   eval      waggior='S'
014100160824    6c                   endif
014200160824    5c                   endif
014300160822
014400160822     c                   leave
014500160822    4c                   endif
014600160822     c
014700160822     c     karb          readpe    fnevb01l
014800160822    3c                   enddo
014900160822
015000160822    3c                   if        weve_tc=' '
015100160822     c                   eval      waggior='S'
015200160822    3c                   endif
015300160822    2c                   endif
015400050221     c
015500071026     C* AGGIORNO DATA DI STAMPA
015600160822    2c                   if        waggior='S'
015700160822     C                   MOVE      ILGC4DAT      GCPDLMa
015800160822     c                   except    testar
015900160822
016000160822     c                   setoff                                       01
016100160822     c                   if        *in01 and not *in01
016200160822     C                   EXCEPT    AGG5
016300160822    2c                   endif
016400160822
016500160822    2c                   endif
016600071011     C*
016700160822    1c                   enddo
016800071011
016900160822    0c                   enddo
017000160822     c
017100160822     c                   exsr      Aggiotab
017200160822
017300071011     c                   seton                                        lr
017400160822     C*---------------------------------------------------------------*
017500160822     c     Aggiotab      BEGSR
017600160822
017700160822     c                   Eval      kTbeCod = 'SDF'
017800160822     c                   Movel(p)  ilgc4lna      kTbeKe1
017900160822     c     Ktbe          Chain     Tntbe01l
018000160822    2c                   If        %Found(Tntbe01l)
018100160822     c                   Movel     TbeUni        dSdf
018200160822     c                   monitor
018300160822    3c                   If        §SDFD_IMG < ilgc4dat
018400160822     c                   Z-Add     ilgc4dat      §SDFD_IMG
018500160822     c                   eval      §SDFE_IMG='1'
018600160822     c                   else
018700160822     c                   select
018800160822     c                   when      §SDFE_IMG<='1'
018900160822     c                   eval      §SDFE_IMG='2'
019000160822     c                   other
019100160822     c                   eval      §SDFE_IMG='3'
019200160822     c                   endsl
019300160822     c
019400160822    3c                   EndIf
019500160822
019600160822     c                   on-error
019700160822     c                   Z-Add     ilgc4dat      §SDFD_IMG
019800160822     c                   eval      §SDFE_IMG='1'
019900160822     c                   endmon
020000160822
020100160822     c                   Movel     dSdf          TbeUni
020200160822     c                   Update    Tntbe000
020300160822   x2c                   else
020400160822
020500160822     c                   clear                   tntbe000
020600160822     c                   eval      tbeapl='OP'
020700160822     c                   eval      tbecod='SDF'
020800160822     c                   eval      tbeke1=%editc(ilgc4lna:'X')
020900160822     c                   clear                   dsdf
021000160822     c                   Z-Add     ilgc4dat      §SDFd_IMG
021100160822     c                   eval      §SDFE_IMG='1'
021200160822     c                   Movel     dSdf          TbeUni
021300160822     c                   write     Tntbe000
021400160822     c                   endif
021500160822     c
021600160822     C                   ENDSR
021700050221     C*---------------------------------------------------------------*
021800050221     C     *INZSR        BEGSR
021900050221     C*---------------------------------------------------------------*
022000160822     c
022100160822     c* Carico i codici di mancata consegna
022200160822     c                   eval      kcod='2A'
022300160822     c                   eval      codut=1
022400160822     c                   clear                   xx
022500160822     c                   open      tabel00f
022600160822     c     ktab          setll     tabel00f
022700160822     c     ktab          reade     tabel00f
022800160822     c                   dow       not %eof(tabel00f)
022900160822     c                   eval      ds2a=tbluni
023000160822     c                   if        §2antc='S'
023100160822     c                   add       1             xx                3 0
023200160823     c                   movel     tblkey        codntc(xx)
023300160822     c                   endif
023400160822     c     ktab          reade     tabel00f
023500160822     c                   enddo
023600160822     c                   close     tabel00f
023700160822     c
023800050221      *
023900050221     C* DEFINIZIONE CHIAVI
024000160822     c     Ktbe          Klist
024100160822     c                   Kfld                    kTbeCod
024200160822     c                   Kfld                    kTbeKe1
024300050221     C     KGCp          KLIST
024400160819     C                   KFLD                    ilgc4lna
024500160819     C                   KFLD                    kFAS              3 0
024600160822     C     Ktab          KLIST
024700160822     C                   KFLD                    codut             1 0
024800160822     C                   KFLD                    kcod              2
024900050221     C*
025000050221     C     KARB          KLIST
025100160822     C                   KFLD                    GCPAASa
025200050412     C                   KFLD                    GCPLNPa
025300050412     C                   KFLD                    GCPNRSa
025400050412     C                   KFLD                    GCPNSPa
025500160824     c*
025600160824     c                   movel     datasys       dateu             8 0
025700071026     C*
025800160822     C                   ENDSR
025900160822     C*---------------------------------------------------------------*
026000050221     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
026100160822     C*---------------------------------------------------------------*
026200160822     c     Car£6         BEGSR
026300050221     c                   clear                   trul06ds
026400050221     c                   eval      d06cod = '£6'
026500160819     c                   movel     ilgc4lna      d06key
026600050221     c                   movel     trul06ds      kpjbu
026700050221     c                   call      'TRUL06R'
026800050221     c                   parm                    kpjba
026900050221     c                   movel     kpjbu         trul06ds
027000050221     c                   movea     lin           fil£6
027100050221     C                   ENDSR
027200050221     C*****************************************************************
027300050221     Otigcp000  E            AGG5
027400050221     O                       GCPDLMa
027500160822     o
027600160822     OPRTF198   E            TESTAR      1
027700160822     O                                         +  0 'giacenze aggiornate'
027800160822     o                       gcpagc
027900160822     o                       gcpfgc           +   1
028000160822     o                       gcpngc        z  +   1
028100160822     o                       GCPaasA       z  +   1
028200160822     o                       GCPLNPA       z  +   1
028300160822     o                       GCPnrsA       z  +   1
028400160822     o                       GCPnspA       z  +   1
