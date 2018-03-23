000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400070731     F*          INTERROGAZIONE STORICO autisti            *
000401070731     FTRmz48D   CF   E             WORKSTN
000402070731     F                                     SFILE(GA10DSF1:NRR)
001000910507     FTABEL00F  IF   E           K DISK
001100070731     Fmzxnh01L  IF   E           K DISK
001701011129     D DTAiso          S               D   INZ
001702011129     D DTAEUR          S               D   DATFMT(*EUR) INZ
002400000000     D KPJBA         E DS
002401070731     D trmz47ds      E DS
002402070731     D dsxn          E DS
002700000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002701070731     D fld             S              1
006901011129     c                   exsr      clr01
007000011129     C  nlr              do        *hival
007001011130     c                   EXSR      CLR00
007002011130     c                   if        *in99
007003011130     c                   setoff                                       99
007004011130     c                   write     ga10d001
007005011130     c                   seton                                        99
007006011130     c                   end
007800000000     C                   EXFMT     GA10D001
007900011129     C* FINE LAVORO
008000011129     C   Kc              leave
008001011130     C* ricerca
008003011130     C                   if        *inkd
008004011130     C                   EXSR      RIC01
008005011130     c                   iter
008006011130     c                   end
008100011129     C* controlli
008101011129     c                   exsr      ctr01
008102011129     c   99              iter
009900000000     C* PULIZIA SUBFILE
010000000000     C                   EXSR      PULIZ
010100000000     C* CARICAMENTO SUBFILE
010200000000     C                   EXSR      CARIC
010201070925     c                   if        nrr <> 0
010400011129     C                   do        *hival
010401031128     C                   WRITE     GA10D002
010500000000     C                   EXFMT     GA10DSC1
010501011129     c   kc              leave
010600011129     C                   if        *inkl
010601011129     c                   exsr      clr01
010602011129     c                   leave
010603011129     c                   end
010700011129     C                   enddo
010701070925     c                   end
010702011129     c   kc              leave
010703011129     C                   enddo
010800000000     C                   SETON                                        LR
010801011130     c**********************************************************************
010802011130     c* pulisce 1° formato
010803011130     c**********************************************************************
010804011130     C     clr00         BEGSR
010805011130     C                   Z-ADD     0             §RIGA
010806011130     C                   Z-ADD     0             §COLON
010807011130     C                   CLEAR                   §FLD
010808011130     C                   ENDSR
010809011130     c**********************************************************************
010810011130     C*  ricerca
010811011130     c**********************************************************************
010812011130     C     ric01         BEGSR
010813011130     c                   movel     '1'           fld
010814011130     c                   select
010841011219     C                   when      §fld = 'VCAU'
010842011219     c                   exsr      srvcau
010843011130     c                   endsl
010844011130     C* POSIZIONAMENTO CURSORE
010845011130     C                   Z-ADD     §RIGA         §RIG
010846011130     C                   Z-ADD     §COLON        §COL
010847011130     C                   ENDSR
010848011219     c**********************************************************************
010849011219     C** autista
010850011219     c**********************************************************************
010851011219     C     srvcau        BEGSR
010852011219     c                   movel     'XN'          parcod
010853011219     c                   movel(P)  VCAU          parkey
010854011219     c                   exsr      srtab
010855011219     c                   movel     parkey        vcau
010856011219     c                   if        *in99
010857011219     C                   MOVEL     *blanks       VDcAU
010858011219     C                   SETON                                        55
010859011219     C                   else
010860011219     C                   MOVEL     TBLUNI        VDcAU
010861011219     C                   END
010862011219     c*
010863011219     C                   ENDSR
010864011219     C**********************************************************************
010865011219     C** Controlli e decodifiche campi con tabel00f
010866011219     C**********************************************************************
010867011219     C     srtab         BEGSR
010868011219     c                   if        fld <> *blanks
010869011219     c                   movel     parkey        parkeys           8
010870011219     C                   CALL      'TRUL19R'
010871011219     c                   parm                    parcod            2
010872011219     c                   parm      '1'           parord            1
010873011219     c                   parm      *blanks       parkey            8
010874011219     c                   parm      *blanks       cmd               1
010875011219     c* ritorno senza scelta
010876011219     c                   if        cmd = 'L'
010879011219     c                   movel     parkeys       parkey
010880011219     C                   END
010881011219     C                   END
010882011219     C                   MOVEL     *blanks       tbluni
010883011219     C                   MOVEL     parcod        COD
010884011219     C                   MOVEL(p)  parkey        KEY
010885011219     C     KTAB          CHAIN     TABEL                              99
010886011219     c*
010887011219     C                   ENDSR
010900000000     C******************************************************
011200000000     C** PULIZIA DEL SUBFILE
011201011129     C******************************************************
011301011129     C     PULIZ         BEGSR
011400000000     C                   SETON                                        70
011500000000     C                   WRITE     GA10DSC1
011600000000     C                   SETOFF                                       70
011700000000     C                   ENDSR
011800000000     C******************************************************
012100070731     C** decodifica autista
012201011129     C******************************************************
012202070731     C     decau         BEGSR
012203070731     C                   MOVEL     'XN'          COD
012206070731     C     KTAB          CHAIN     TABEL                              22
012207070731     C  N22              MOVEL     TBLUNI        dsxn
012208070731     C   22              clear                   dsxn
012209070731     C                   ENDSR
012210070731     C******************************************************
012211070731     C** CARICAMENTO SUBFILE
012212070731     C******************************************************
012213070731     C     CARIC         BEGSR
012214070731     c                   if        vcau <> ' '
013002070731     C     vcau          SETLL     mzxnh01l
013003070731     c                   else
013004070731     C     *loval        SETLL     mzxnh01l
013005070731     c                   end
013300000000     C                   Z-ADD     0             NRR               4 0
013400000000     C                   DO        *HIVAL
013401070731     c                   if        vcau <> ' '
014103070731     C     vcau          READE     mzxnh01l
014104070731     c                   else
014105070731     C                   read      mzxnh01l
014106070731     c                   end
014107070731     c                   if        %eof(mzxnh01l) or nrr = 9999
014108070731     c                   leave
014109070731     c                   end
014400910507     C* DECODIFICA AUTISTA
014401070731     C                   MOVEL     xnhCAU        s1cau
014402070731     C                   MOVEL(p)  xnhCAU        KEY
014403070731     c                   exsr      decau
014404070731     c                   movel     §xndes        caudes
016500910507     C* GIRO DATA VARIAZIONE
016600070731     C                   MOVE      xnhDTA        Dtaiso
016700011129     C                   move      dtaiso        dtaeur
016800011129     C                   move      Dtaeur        S1DTA
016801070731     C                   MOVE      xnhsoc        s1soc
016802070731     C                   MOVE      xnhmat        s1mat
017300910507     C*
017301070907     c                   if        nrr < 9999
017400070731     C                   ADD       1             NRR
017500070731     C                   WRITE     GA10DSF1
017501070907     c                   else
017502070907     c                   leave
017503070907     c                   end
017600070731     C                   END
017700000000     C                   ENDSR
020700011129     C*----------------------------------------------------*
020800011129     c     ctr01         begsr
020900011129     c                   setoff                                       99
023102070731     C                   MOVEL     *BLANKS       Vdcau
023105070731     C* DECODIFICA AUTISTA
023106070731     c                   if        vcau <> ' '
023107070731     C                   MOVEL(p)  vcau          KEY
023108070731     c                   exsr      decau
023109070731     c                   movel     §xndes        vdcau
023110070731     C     vcau          CHAIN     mzxnh01l                           43
023111011219     C   43              seton                                        99
023112011219     C                   END
023200011129     c                   endsr
023300011129     C*----------------------------------------------------*
023400011129     c     clr01         begsr
023401070907     C                   MOVEL     *BLANKS       Vdcau
023904070907     c                   move      kpjbu         vcau
023905070907     c                   if        vcau <> ' '
023906070907     c                   exsr      srvcau
023907070907     c                   end
024000011129     c                   endsr
024001070731     C*----------------------------------------------------*
024041011129     c     *inzsr        begsr
024042011129     C     *ENTRY        PLIST
024043011129     C                   PARM                    KPJBA
024044011129     C                   Z-ADD     1             CODUT
024045011129     C                   CALL      'XPARUT'
024046011129     C                   PARM                    UTEDSE
024047011129     C     CODUT         ifeq      -1
024048011129     c                   seton                                        lr
024049011129     c                   end
024050011129     C                   MOVEL     RAGUT         RSUT             20
024051011129     C                   MOVEL     RAGUT         RSUT2            25
024052011129     C*----------------------------------------------------*
024063011129     C* POSIZIONAMENTO TABELLE                             *
024064011129     C     KTAB          KLIST                                                  *
024065011129     C                   KFLD                    CODUT                          *
024066011129     C                   KFLD                    COD               2            *
024067011129     C                   KFLD                    KEY               8            *
024074011129     c                   endsr
024100011129     C*----------------------------------------------------*
