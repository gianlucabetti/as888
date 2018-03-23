000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300170921      *  DRIVER per controllo DEPOT VERSIONE CAPPARIO ANAGR./LEGAME
000400170921     H*--------------------------------------------------------------*
000500170921      **
000600170921      **  Ricevuti : 1) data 0 = UDATE oppure data >0 x reperire la VERSIONE
000700170921      **             2) Depot di Riferimento a 4  (se si ha il 4 solo questo)
000800170921      **             3) Depot di Riferimento a 7  (se si ha il 7 solo questo)
000900170921      **
001000170921      **  Ritorno  : 4) Flag Versione -> 0 = "OK" / 1 = "Non trovata Versione"
001100170921      **             5) Flag Cappario -> 0 = "OK" / 1 = "Non trovato Depot su Cappario"
001200170921      **             6) Flag Legame   -> 0 = "OK" / 1 = "Non trovato Depot su Legame"
001300170921      **             7) imposta il record ANAGRAFICO del cappario DPCDP10F
001400170921      **             8) imposta il record LEGAME Depot/cliente    DPLEG10F
001500170921      **
001600170921     H*--------------------------------------------------------------*
001700170921      *-- Depot a 7
001800170921     FDPcdp11i  iF   E           K DISK    usropn
001900170921     F                                     rename(DPCDP100:DPCDP11)
002000170921     FDPLEG11L  iF   E           K DISK    usropn
002100170921     F                                     rename(DPLEG000:DPLEG11)
002200170921     H*--------------------------------------------------------------*
002300170921      *-- Depot a 4
002400170921     FDPcdp12i  iF   E           K DISK    usropn
002500170921     F                                     rename(DPCDP100:DPCDP12)
002600170921     FDPLEG14L  iF   E           K DISK    usropn
002700170921     F                                     rename(DPLEG000:DPLEG14)
002800170921      *--------------------------------------------------------------*
002900170921     D WLBDA8          DS                  INZ
003000170921     D  G08DAT                 1      8  0
003100170921     D  G08INV                 9     16  0
003200170921     D  G08ERR                17     17
003300170921     D  G08TGI                18     22  0
003400170921      *--------------------------------------------------------------*
003500170922     dO_DPCDP10Fds   E ds                  extname(DPCDP10F)
003600170922     d                                     Prefix(DPD)
003700170922     dO_DPLEG10Fds   E ds                  extname(DPLEG10F)
003800170922     d                                     Prefix(BRT)
003900170921     D*------------------------------------------------------------------------*
004000170925     d TISIEdepDS    E ds
004100170921     D*------------------------------------------------------------------------*
004200170921     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
004300170921     C*------------------------------------------------------------------------*
004400170922      *    VERIFICA VERSIONE / ANAGRAFICA DPD / ANAGRAFICA LAGAMI CLIENTI_DEPOT
004500120531     C*------------------------------------------------------------------------*
004600170921     c     *Entry        plist
004700170925     c                   parm                    tisiEDEPDS
004800170922     c                   parm                    O_DPCDP10Fds
004900170922     c                   parm                    O_DPLEG10Fds
005000170922      **  lo accende
005100170922     C                   SETON                                        LR
005200170922      **
005300170921     c     Kcdp4         klist
005400170921     c                   kfld                    CDPATB
005500170921     c                   kfld                    CDPVER
005600170921     c                   kfld                    CDPDSTR
005700170921      **
005800170921     c     Kcdp7         klist
005900170921     c                   kfld                    CDPATB
006000170921     c                   kfld                    CDPVER
006100170921     c                   kfld                    CDPDPTN
006200170921      **
006300170921      **  se per errore è stato mandato sul Depot di 7 il Depot di 4
006400170921     c                   if        I_Depot4 =0 and I_Depot7 <= 9999
006500170921     c                   z-add     I_Depot7      I_Depot4
006600170921     c                   z-add     0             I_Depot7
006700170921     c                   end
006800170921      *
006900170921     c                   clear                   O_FlgVers
007000170921     c                   clear                   O_FlgCapp
007100170921     c                   clear                   O_FlgLega
007200170915      **
007300170921      ** il Depot ricevuto è a 4
007400170921     c                   if        I_Depot4 >0 and I_Depot7 =0
007500170921     c                   open      DPcdp12i
007600170921     c                   open      DPLEG14L
007700170921     c                   else
007800170921      ** il Depot ricevuto ha la B.U. quindi è a 7
007900170921     c                   open      DPcdp11i
008000170921     c                   open      DPLEG11L
008100170921     c                   end
008200170921      **
008300170921     C* CALCOLA LA DATA CORRENTE
008400170921     C                   time                    wn14             14 0
008500170921     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
008600170921     C                   movel     wn14          oracor            6 0          *ORA
008700170921     C*
008800170921     C                   z-add     wn8           g08dat
008900170921     C                   z-add     *zeros        g08inv
009000170921     C                   movel     '0'           g08err
009100170921     C                   call      'XSRDA8'
009200170921     C                   parm                    wlbda8
009300170921     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
009400170921      *
009500170921      *   imposta la data per reperire la versione
009600171009     C                   clear                   ISID1DRI
009700170921    1c                   z-add     I_DATAver     iSID1dri
009800170921    1c                   if        iSID1dri = 0
009900170921     C                   z-add     datCOR        iSID1dri
010000170922     C                   z-add     datCOR        I_DATAver
010100170921     c                   end
010200170921      * recupero  versione di cappario
010300170921     C                   clear                   OSID1VER
010400170921     C                   clear                   OSID1VERD
010500170921     C                   clear                   OSID1DDE
010600170921     C                   clear                   OSID1DSC
010700170921     C                   clear                   OSID1BCID
010800170921     c                   call      'TISID1R'
010900170921     C                   PARM                    ISID1DRI
011000170921     C                   PARM                    OSID1VER
011100170921     C                   PARM                    OSID1VERD
011200170921     C                   PARM                    OSID1DDE
011300170921     C                   PARM                    OSID1DSC
011400170921     C                   PARM                    OSID1BCID
011500170921      * se NON TROVATA VERSIONE CAPPARIO
011600170922     c                   move      '0'           O_FLGVERS
011700170921     c                   if        osid1ver <= 0
011800170921     c                   move      '1'           O_FLGVERS
011900170921     c                   RETURN
012000170921     c                   end
012100170921      ** ------
012200170921      **  Controlla   presenza in ANAGRAFICA CAPPARIO DPD
012300170921     c                   move      *blank        CDPATB
012400170921     c                   z-add     oSID1VER      CDPVER
012500170922     c                   move      '0'           O_FlgCapp
012600170921      ****   Depot (4)
012700170921     c                   if        I_Depot4 >0 and I_Depot7 =0
012800170921     c                   movel     I_Depot4      CDPDSTR
012900170921     c     Kcdp4         chain     dpcdp12i
013000170921      * non esiste ANAGRAFICA su CAPPARIO DPD
013100170921     c                   if        Not %Found(dpcdp12i)
013200170921     c                   move      '1'           O_FlgCapp
013300170921     c                   RETURN
013400170922     c                   end
013500170921      ****
013600170921     c                   else
013700170921      ****   Depot (7)
013800170921     c                   movel     I_Depot7      CDPDPTN
013900170921     c     Kcdp7         chain     dpcdp11i
014000170921      * non esiste ANAGRAFICA su CAPPARIO DPD
014100170921     c                   if        Not %Found(dpcdp11i)
014200170921     c                   move      '1'           O_FlgCapp
014300170921     c                   RETURN
014400170921     c                   end
014500170921      ****
014600170921     c                   end
014700170921      ** ------
014800170921      **  Controlla presenza del DEPOT fra i Legami
014900170921      ** ------
015000170921      ****   Depot (4)
015100170921     c                   if        I_Depot4 >0 and I_Depot7 =0
015200170921      ****
015300170921     c                   movel     I_Depot4      legDPC
015400170921     c                   move      '1'           O_FlgLega
015500170921     c     legDPC        setll     dpleg14l
015600170921     c     legDPC        reade     dpleg14l
015700170921      * non esiste ANAGRAFICA su CAPPARIO DPD
015800170921     c                   DOw       not %EoF(dpleg14l)
015900170921     c                   if        legDDE <= iSID1dri and
016000170921     c                             legDSC >= iSID1dri
016100170922     c                   move      '0'           O_FlgLega
016200170922     c                   leave
016300170921     c                   end
016400170921     c     legDPC        reade     dpleg14l
016500170921     c                   endDO
016600170921      ****
016700170921     c                   else
016800170921      ****   Depot (7)
016900170921     c                   movel     I_Depot7      legDep7
017000170921     c                   move      '1'           O_FlgLega
017100170921     c     legDEP7       setll     dpleg11l
017200170921     c     legDEP7       reade     dpleg11l
017300170921      * non esiste ANAGRAFICA su CAPPARIO DPD
017400170921     c                   DOw       not %EoF(dpleg11l)
017500170921     c                   if        legDDE <= iSID1dri and
017600170921     c                             legDSC >= iSID1dri
017700170921     c                   move      '0'           O_FlgLega
017800170922     c                   leave
017900170921     c                   end
018000170921     c     legDEP7       reade     dpleg11l
018100170921     c                   endDO
018200170921      ****
018300170921     c                   end
018400170922      *
018500170922      ****----------------------------------------
018600170922      **   reimposta i campi DEPOT
018700170922     c                   movel     CDPDSTR       I_Depot4
018800170922     c                   movel     CDPDPTN       I_Depot7
018900170922      *
019000170922      ****----------------------------------------
019100170922      *  Controlla se richiamato solo con la DS
019200170922      ****----------------------------------------
019300170922     c                   if        %Parms = 2 or %Parms = 3
019400170922     c                   clear                   O_DPCDP10Fds
019500170922     c                   movel     CDPATB        DPDCDPATB
019600170922     c                   z-add     CDPVER        DPDCDPVER
019700170922     c                   movel     CDPDPTN       DPDCDPDPTN
019800170922     c                   movel     CDPDSTR       DPDCDPDSTR
019900170922     c                   movel     CDPIATA       DPDCDPIATA
020000170922     c                   movel     CDPGRPID      DPDCDPGRPID
020100170922     c                   movel     CDPNAME1      DPDCDPNAME1
020200170922     c                   movel     CDPNAME2      DPDCDPNAME2
020300170922     c                   movel     CDPSTRET      DPDCDPSTRET
020400170922     c                   movel     CDPSTNBR      DPDCDPSTNBR
020500170922     c                   movel     CDPADDR1      DPDCDPADDR1
020600170922     c                   movel     CDPADDR2      DPDCDPADDR2
020700170922     c                   movel     CDPFLOOR      DPDCDPFLOOR
020800170922     c                   movel     CDPBUILD      DPDCDPBUILD
020900170922     c                   movel     CDPISO2       DPDCDPISO2
021000170922     c                   movel     CDPSTATE      DPDCDPSTATE
021100170922     c                   movel     CDPPTC        DPDCDPPTC
021200170922     c                   movel     CDPCITY       DPDCDPCITY
021300170922     c                   movel     CDPTEL        DPDCDPTEL
021400170922     c                   movel     CDPFAX        DPDCDPFAX
021500170922     c                   movel     CDPEMAIL      DPDCDPEMAIL
021600170922     c                   movel     CDPWEB        DPDCDPWEB
021700170922     c                   z-add     CDPBUCN       DPDCDPBUCN
021800170922     c                   movel     CDPGEOX       DPDCDPGEOX
021900170922     c                   movel     CDPGEOY       DPDCDPGEOY
022000170922     c                   movel     CDPDUMY       DPDCDPDUMY
022100170922     c                   end
022200170922      *
022300170922     c                   if        %Parms = 3
022400170922     c                   clear                   O_DPLEG10Fds
022500170922     c                   movel     LEGDPC        BRTLEGDPC
022600170922     c                   movel     LEGNAR        BRTLEGNAR
022700170922     c                   z-add     LEGKSC        BRTLEGKSC
022800170922     c                   z-add     LEGDDE        BRTLEGDDE
022900170922     c                   z-add     LEGDSC        BRTLEGDSC
023000170922     c                   z-add     LEGDTV        BRTLEGDTV
023100170922     c                   z-add     LEGORV        BRTLEGORV
023200170922     c                   movel     LEGPRU        BRTLEGPRU
023300170922     c                   movel     LEGORMA       BRTLEGORMA
023400170922     c                   movel     LEGATB        BRTLEGATB
023500170922     c                   movel     LEGFLO        BRTLEGFLO
023600170922     c                   movel     LEGDEP7       BRTLEGDEP7
023700170922     c                   z-add     LEGBUN        BRTLEGBUN
023800170922     c                   end
023900170922      ****
024000081110     c                   return
024100090424     C*---------------------------------------------------------------*
