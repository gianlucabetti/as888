000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090123     Ftivgdtmp  uF   E             DISK
000500091222     Ftivawwwt  uf a E             DISK
000600091222     Ftitas30c  iF   E           K DISK
000700991027
000800090203     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090203
001300090203
001400090203     D ds_body         ds
001500091222     D  ds_b_tracer                  20a   inz
001600091222     D  ds_b_lotto                   12a   inz
001700091222     D  ds_b_datoraev                14a   inz
001800091222     D  ds_b_codmitt                 12a   inz('000000200003')
001900091222     D  ds_b_proven                  12a   inz('000000200003')
002000091222     D  ds_b_destinaz                12a   inz('000000200003')
002100091222     D  ds_b_articolo                20a   inz
002200091222     D  ds_b_destinat                40a   inz
002300091222     D  ds_b_indirizz                30a   inz
002400091222     D  ds_b_cap                      5a   inz
002500091222     D  ds_b_localita                25a   inz
002600091222     D  ds_b_prov                     2a   inz
002700091222     D  ds_b_telefono                12a   inz
002800091222     D  ds_b_divisa                   1a   inz('E')
002900091222     D  ds_b_contrass                 9a   inz
003000091222     D  ds_b_codcaus                  3a   inz('906')
003100091222     D  ds_b_tiprec                   1a   inz
003200091222     D  ds_b_stscons                  3a   inz
003300091222     D  ds_b_codpenal                 3a   inz
003400091222     D  ds_b_codvers                 12a   inz
003500091222     D  ds_b_filler01                10a   inz
003600091222     D  ds_b_codcorr                  2a   inz('26')
003700091222     D  ds_b_datacons                 8a   inz
003800091222     D  ds_b_codgiac                  3a   inz
003900090203
004000090203     D ds_Key_bolla    ds
004100091222     D  wvasAAS                       4a
004200091222     D  wvasLNP                       3a
004300091222     D  wvasNRS                       2a
004400091222     D  wvasNSP                       7a
004500100121
004600100121     D ds_data         ds             8
004700100121     D  wAAAA                         4a
004800100121     D  wMM                           2a
004900100121     D  wGG                           2a
005000090123
005100091222     D  wCAS           s              9s 2 inz
005200090203
005300991027
005400991027     C                   EXSR      traduci
005500921023     C                   SETON                                        LR
005600030908
005700090203
005800090123
005900991027     C     traduci       BEGSR
006000991027     C*
006100090203     C* Se tutto ok => procedo
006200090203     C                   IF        wrkesito = *blanks
006300090123     C                   READ      tivgdtmp
006400090203     C                   movel     vgddta        tnvasds
006500090203     C*
006600090123     C                   DOW       not %eof(tivgdtmp)
006700100209     C*
006800100209     C* Filtro x le spedizioni aventi RMA lungo almeno 10 byte e che inizi con '5' o '7'
006900100211     C                   if        %len(%trim(vasRMA)) >= 10 AND
007000100211     C                             (%subst(%trim(vasRMA):1:1) = '5' OR
007100100211     C                              %subst(%trim(vasRMA):1:1) = '7')
007200091222     C*
007300091222     C* Elaboro solamente i contrassegni realmente pagati
007400091222     C                   if        VASFPC <> 'A' and
007500091222     C                             VASFPC <> 'S'
007600091222     C*
007700091222     C* "Chaino" la bolla x reperire i dati mancanti
007800091222     C     KEYtas30_P    CHAIN     titas30c
007900091222     C                   if        %found(titas30c)
008000090203     C*
008100090203     C* Valorizzo il record d dettaglio corrente
008200090203     C                   exsr      wriBody
008300091222     C*
008400091222     C                   endif
008500091222     C*
008600091222     C                   endif
008700100211     C                   endif
008800930409     C*
008900090123     C                   DELETE    tivgd000
009000090123     C                   READ      tivgdtmp
009100090203     C                   movel     vgddta        tnvasds
009200090203     C*
009300030325     C                   ENDDO
009400090203     C*
009500090203     C                   EVAL      wrkesito = '0'
009600090203     C*
009700090203     C                   ENDIF
009800991027     C*
009900910830     C                   ENDSR
010000090203
010100091222
010200091222
010300090203     C     wriBody       BEGSR
010400090203     C*
010500090203     C* Inizializzo la ds d dettaglio
010600090203     C                   reset                   ds_body
010700090203     C*
010800090203     C                   movel     vasAAS        wvasAAS
010900090203     C                   movel     vasLNP        wvasLNP
011000090203     C                   movel     vasNRS        wvasNRS
011100090203     C                   movel     vasNSP        wvasNSP
011200090203     C*
011300100208     C***                eval      ds_b_tracer   = %char(vasRMN)
011400100208     C                   eval      ds_b_tracer   = vasRMA
011500100121     C                   movel(p)  vasDDP        ds_data
011600100121     C                   eval      ds_b_datoraev = wGG   +
011700100121     C                                             wMM   +
011800100121     C                                             wAAAA + '120000'
011900091222     C                   eval      ds_b_articolo = ds_key_bolla
012000091222     C                   eval      ds_b_destinat = tasRSD
012100091222     C                   eval      ds_b_indirizz = tasIND
012200091222     C                   eval      ds_b_cap      = tasCAD
012300091222     C                   eval      ds_b_localita = tasLOD
012400091222     C                   eval      ds_b_prov     = tasPRD
012500091222     C                   z-add     vasCAS        wCAS
012600091222     C                   movel(p)  wCAS          ds_b_contrass
012700091222     C                   eval      ds_b_codvers  = %char(vasNDP)
012800090203     C*
012900090203     C                   clear                   tivaw000
013000090203     C*
013100091222     C                   eval      vawdta = ds_body
013200090205     C                   write     tivaw000
013300090203     C*
013400090203     C                   ENDSR
013500090203
013600090203
013700090203
013800090123
013900991027      /TITLE Operazioni iniziali.
014000991027     C     *inzsr        BEGSR
014100991027     C*
014200991027     C     *ENTRY        PLIST
014300991027     C                   parm                    prmppt
014400991027     C     wrkesito      parm      wrkesito      prmesito
014500091222     C*
014600091222     C****
014700091222     C* Definizione chiavi
014800091222     C****
014900091222     C*
015000091222     C* Chiave su TITAS30C - parziale
015100091222     C     KEYtas30_P    KLIST
015200091222     C                   KFLD                    vasAAS
015300091222     C                   KFLD                    vasLNP
015400091222     C                   KFLD                    vasNRS
015500091222     C                   KFLD                    vasNSP
015600991027     C*
015700991027     C                   ENDSR
