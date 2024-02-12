;;;; sndfile.lisp

(in-package :mushub)

(define-foreign-library libsndfile
  (:unix (:or "libsndfile.so.1" "libsndfile.so" "sndfile.so"))
  (t (:default "sndfile")))

(use-foreign-library libsndfile)

;; ----------------------------------------------------------------------------
;; CONSTANTS
;; ----------------------------------------------------------------------------

(defconstant SF_FALSE                             #x0)
(defconstant SF_TRUE                              #x1)
(defconstant SFM_READ                             #x10)
(defconstant SFM_WRITE                            #x20)
(defconstant SFM_RDWR                             #x30)
(defconstant SF_AMBISONIC_NONE                    #x40)
(defconstant SF_AMBISONIC_B_FORMAT                #x41)

(defconstant SF_FORMAT_WAV                        #x010000)   ; Microsoft WAV format (little endian).
(defconstant SF_FORMAT_AIFF                       #x020000)   ; Apple/SGI AIFF format (big endian).
(defconstant SF_FORMAT_AU                         #x030000)   ; Sun/NeXT AU format (big endian).
(defconstant SF_FORMAT_RAW                        #x040000)   ; RAW PCM data.
(defconstant SF_FORMAT_PAF                        #x050000)   ; Ensoniq PARIS file format.
(defconstant SF_FORMAT_SVX                        #x060000)   ; Amiga IFF / SVX8 / SV16 format.
(defconstant SF_FORMAT_NIST                       #x070000)   ; Sphere NIST format.
(defconstant SF_FORMAT_VOC                        #x080000)   ; VOC files.
(defconstant SF_FORMAT_IRCAM                      #x0A0000)   ; Berkeley/IRCAM/CARL
(defconstant SF_FORMAT_W64                        #x0B0000)   ; Sonic Foundry’s 64 bit RIFF/WAV
(defconstant SF_FORMAT_MAT4                       #x0C0000)   ; Matlab (tm) V4.2 / GNU Octave 2.0
(defconstant SF_FORMAT_MAT5                       #x0D0000)   ; Matlab (tm) V5.0 / GNU Octave 2.1
(defconstant SF_FORMAT_PVF                        #x0E0000)   ; Portable Voice Format
(defconstant SF_FORMAT_XI                         #x0F0000)   ; Fasttracker 2 Extended Instrument
(defconstant SF_FORMAT_HTK                        #x100000)   ; HMM Tool Kit format

(defconstant SF_FORMAT_SDS                        #x110000)   ; Midi Sample Dump Standard
(defconstant SF_FORMAT_AVR                        #x120000)   ; Audio Visual Research
(defconstant SF_FORMAT_WAVEX                      #x130000)   ; MS WAVE with WAVEFORMATEX
(defconstant SF_FORMAT_SD2                        #x160000)   ; Sound Designer 2
(defconstant SF_FORMAT_FLAC                       #x170000)   ; FLAC lossless file format
(defconstant SF_FORMAT_CAF                        #x180000)   ; Core Audio File format
(defconstant SF_FORMAT_WVE                        #x190000)   ; Psion WVE format
(defconstant SF_FORMAT_OGG                        #x200000)   ; Xiph OGG container
(defconstant SF_FORMAT_MPC2K                      #x210000)   ; Akai MPC 2000 sampler
(defconstant SF_FORMAT_RF64                       #x220000)   ; RF64 WAV file
(defconstant SF_FORMAT_MPEG                       #x230000)   ; MPEG-1/2 audio stream

(defconstant SF_FORMAT_PCM_S8                     #x0001)     ; Signed 8 bit data
(defconstant SF_FORMAT_PCM_16                     #x0002)     ; Signed 16 bit data
(defconstant SF_FORMAT_PCM_24                     #x0003)     ; Signed 24 bit data
(defconstant SF_FORMAT_PCM_32                     #x0004)     ; Signed 32 bit data
(defconstant SF_FORMAT_PCM_U8                     #x0005)     ; Unsigned 8 bit data (WAV and RAW only)
(defconstant SF_FORMAT_FLOAT                      #x0006)     ; 32 bit float data
(defconstant SF_FORMAT_DOUBLE                     #x0007)     ; 64 bit float data
(defconstant SF_FORMAT_ULAW                       #x0010)     ; U-Law encoded.
(defconstant SF_FORMAT_ALAW                       #x0011)     ; A-Law encoded.
(defconstant SF_FORMAT_IMA_ADPCM                  #x0012)     ; IMA ADPCM.
(defconstant SF_FORMAT_MS_ADPCM                   #x0013)     ; Microsoft ADPCM.
(defconstant SF_FORMAT_GSM610                     #x0020)     ; GSM 6.10 encoding.
(defconstant SF_FORMAT_VOX_ADPCM                  #x0021)     ; OKI / Dialogix ADPCM
(defconstant SF_FORMAT_NMS_ADPCM_16               #x0022)     ; 16kbs NMS G721-variant encoding.
(defconstant SF_FORMAT_NMS_ADPCM_24               #x0023)     ; 24kbs NMS G721-variant encoding.
(defconstant SF_FORMAT_NMS_ADPCM_32               #x0024)     ; 32kbs NMS G721-variant encoding.
(defconstant SF_FORMAT_G721_32                    #x0030)     ; 32kbs G721 ADPCM encoding.
(defconstant SF_FORMAT_G723_24                    #x0031)     ; 24kbs G723 ADPCM encoding.
(defconstant SF_FORMAT_G723_40                    #x0032)     ; 40kbs G723 ADPCM encoding.
(defconstant SF_FORMAT_DWVW_12                    #x0040)     ; 12 bit Delta Width Variable Word encoding.
(defconstant SF_FORMAT_DWVW_16                    #x0041)     ; 16 bit Delta Width Variable Word encoding.
(defconstant SF_FORMAT_DWVW_24                    #x0042)     ; 24 bit Delta Width Variable Word encoding.
(defconstant SF_FORMAT_DWVW_N                     #x0043)     ; N bit Delta Width Variable Word encoding.
(defconstant SF_FORMAT_DPCM_8                     #x0050)     ; 8 bit differential PCM (XI only)
(defconstant SF_FORMAT_DPCM_16                    #x0051)     ; 16 bit differential PCM (XI only)
(defconstant SF_FORMAT_VORBIS                     #x0060)     ; Xiph Vorbis encoding.
(defconstant SF_FORMAT_OPUS                       #x0064)     ; Xiph/Skype Opus encoding.
(defconstant SF_FORMAT_ALAC_16                    #x0070)     ; Apple Lossless Audio Codec (16 bit).
(defconstant SF_FORMAT_ALAC_20                    #x0071)     ; Apple Lossless Audio Codec (20 bit).
(defconstant SF_FORMAT_ALAC_24                    #x0072)     ; Apple Lossless Audio Codec (24 bit).
(defconstant SF_FORMAT_ALAC_32                    #x0073)     ; Apple Lossless Audio Codec (32 bit).
(defconstant SF_FORMAT_MPEG_LAYER_I               #x0080)     ; MPEG-1 Audio Layer I.
(defconstant SF_FORMAT_MPEG_LAYER_II              #x0081)     ; MPEG-1 Audio Layer II.
(defconstant SF_FORMAT_MPEG_LAYER_III             #x0082)     ; MPEG-2 Audio Layer III.

(defconstant SF_ENDIAN_FILE                       #x00000000) ; Default file endian-ness.
(defconstant SF_ENDIAN_LITTLE                     #x10000000) ; Force little endian-ness.
(defconstant SF_ENDIAN_BIG                        #x20000000) ; Force big endian-ness.
(defconstant SF_ENDIAN_CPU                        #x30000000) ; Force CPU endian-ness.
(defconstant SF_FORMAT_SUBMASK                    #x0000FFFF)
(defconstant SF_FORMAT_TYPEMASK                   #x0FFF0000)
(defconstant SF_FORMAT_ENDMASK                    #x30000000)

(defconstant SF_STR_TITLE                         #x01)       ; Title.
(defconstant SF_STR_COPYRIGHT                     #x02)       ; Copyright.
(defconstant SF_STR_SOFTWARE                      #x03)       ; Software.
(defconstant SF_STR_ARTIST                        #x04)       ; Artist.
(defconstant SF_STR_COMMENT                       #x05)       ; Comment.
(defconstant SF_STR_DATE                          #x06)       ; Date.
(defconstant SF_STR_ALBUM                         #x07)       ; Album.
(defconstant SF_STR_LICENSE                       #x08)       ; License.
(defconstant SF_STR_TRACKNUMBER                   #x09)       ; Track number.
(defconstant SF_STR_GENRE                         #x10)       ; Genre.

(defconstant SF_ERR_NO_ERROR                      0)
(defconstant SF_ERR_UNRECOGNISED_FORMAT           1)
(defconstant SF_ERR_SYSTEM                        2)
(defconstant SF_ERR_MALFORMED_FILE                3)
(defconstant SF_ERR_UNSUPPORTED_ENCODING          4)

;; CONSTANTS FOR SF-COMMAND
;; ----------------------------------------------------------------------------
(defconstant SFC_GET_LIB_VERSION                  #x1000)
(defconstant SFC_GET_LOG_INFO                     #x1001)
(defconstant SFC_GET_CURRENT_SF_INFO              #x1002)
(defconstant SFC_GET_NORM_DOUBLE                  #x1010)
(defconstant SFC_GET_NORM_FLOAT                   #x1011)
(defconstant SFC_SET_NORM_DOUBLE                  #x1012)
(defconstant SFC_SET_NORM_FLOAT                   #x1013)
(defconstant SFC_SET_SCALE_FLOAT_INT_READ         #x1014)
(defconstant SFC_SET_SCALE_INT_FLOAT_WRITE        #x1015)
(defconstant SFC_GET_SIMPLE_FORMAT_COUNT          #x1020)
(defconstant SFC_GET_SIMPLE_FORMAT                #x1021)
(defconstant SFC_GET_FORMAT_INFO                  #x1028)
(defconstant SFC_GET_FORMAT_MAJOR_COUNT           #x1030)
(defconstant SFC_GET_FORMAT_MAJOR                 #x1031)
(defconstant SFC_GET_FORMAT_SUBTYPE_COUNT         #x1032)
(defconstant SFC_GET_FORMAT_SUBTYPE               #x1033)
(defconstant SFC_CALC_SIGNAL_MAX                  #x1040)
(defconstant SFC_CALC_NORM_SIGNAL_MAX             #x1041)
(defconstant SFC_CALC_MAX_ALL_CHANNELS            #x1042)
(defconstant SFC_CALC_NORM_MAX_ALL_CHANNELS       #x1043)
(defconstant SFC_GET_SIGNAL_MAX                   #x1044)
(defconstant SFC_GET_MAX_ALL_CHANNELS             #x1045)
(defconstant SFC_SET_ADD_PEAK_CHUNK               #x1050)
(defconstant SFC_UPDATE_HEADER_NOW                #x1060)
(defconstant SFC_SET_UPDATE_HEADER_AUTO           #x1061)
(defconstant SFC_FILE_TRUNCATE                    #x1080)
(defconstant SFC_SET_RAW_START_OFFSET             #x1090)

;; Commands reserved for dithering, which is not implemented.
(defconstant SFC_SET_DITHER_ON_WRITE              #x10A0)
(defconstant SFC_SET_DITHER_ON_READ               #x10A1)
(defconstant SFC_GET_DITHER_INFO_COUNT            #x10A2)
(defconstant SFC_GET_DITHER_INFO                  #x10A3)
(defconstant SFC_GET_EMBED_FILE_INFO              #x10B0)
(defconstant SFC_SET_CLIPPING                     #x10C0)
(defconstant SFC_GET_CLIPPING                     #x10C1)
(defconstant SFC_GET_CUE_COUNT                    #x10CD)
(defconstant SFC_GET_CUE                          #x10CE)
(defconstant SFC_SET_CUE                          #x10CF)
(defconstant SFC_GET_INSTRUMENT                   #x10D0)
(defconstant SFC_SET_INSTRUMENT                   #x10D1)
(defconstant SFC_GET_LOOP_INFO                    #x10E0)
(defconstant SFC_GET_BROADCAST_INFO               #x10F0)
(defconstant SFC_SET_BROADCAST_INFO               #x10F1)
(defconstant SFC_GET_CHANNEL_MAP_INFO             #x1100)
(defconstant SFC_SET_CHANNEL_MAP_INFO             #x1101)
(defconstant SFC_RAW_DATA_NEEDS_ENDSWAP           #x1110)

;; Support for Wavex Ambisonics Format
(defconstant SFC_WAVEX_SET_AMBISONIC              #x1200)
(defconstant SFC_WAVEX_GET_AMBISONIC              #x1201)

;; RF64 files can be set so that on-close, writable files that have less
;; than 4GB of data in them are converted to RIFF/WAV, as per EBU
;; recommendations.
(defconstant SFC_RF64_AUTO_DOWNGRADE              #x1210)
(defconstant SFC_SET_VBR_ENCODING_QUALITY         #x1300)
(defconstant SFC_SET_COMPRESSION_LEVEL            #x1301)

;; Ogg format commands
(defconstant SFC_SET_OGG_PAGE_LATENCY_MS          #x1302)
(defconstant SFC_SET_OGG_PAGE_LATENCY             #x1303)
(defconstant SFC_GET_OGG_STREAM_SERIALNO          #x1306)

(defconstant SFC_GET_BITRATE_MODE                 #x1304)
(defconstant SFC_SET_BITRATE_MODE                 #x1305)

;; Cart Chunk support
(defconstant SFC_SET_CART_INFO                    #x1400)
(defconstant SFC_GET_CART_INFO                    #x1401)

;; Opus files original samplerate metadata
(defconstant SFC_SET_ORIGINAL_SAMPLERATE          #x1500)
(defconstant SFC_GET_ORIGINAL_SAMPLERATE          #x1501)

;; Following commands for testing only.
(defconstant SFC_TEST_IEEE_FLOAT_REPLACE          #x6001)

;; ----------------------------------------------------------------------------
;; TYPES & COMPOUND STRUCTURES
;; ----------------------------------------------------------------------------

(defctype sf_count_t :int64)
(defctype size_t     :long)
(defctype SNDFILE    :pointer)

(defcstruct SF_INFO
  (frames      sf_count_t)
  (samplerate  :int)
  (channels    :int)
  (format      :int)
  (sections    :int)
  (seekable    :int))

(defcstruct SF_FORMAT_INFO
  (format      :int)
  (name        :pointer)
  (extension   :pointer))

(defcstruct SF_BROADCAST_INFO
  (description          :char :count 256)
  (originator           :char :count 32)
  (originator_reference :char :count 32)
  (origination_date     :char :count 10)
  (origination_time     :char :count 8)
  (time_reference_low   :uint32)
  (time_reference_high  :uint32)
  (version              :short)
  (umid                 :char :count 64)
  (reserved             :char :count 190)
  (coding_history_size  :uint32)
  (coding_history       :char :count 256))

;; ----------------------------------------------------------------------------
;; C FUNCTIONS
;; ----------------------------------------------------------------------------

(defcfun ("sf_open" sf-open-internal) :pointer
  (path :string)
  (mode :int)
  (sfinfo :pointer))

(defun sf-open (path mode sfinfo)
  (sf-open-internal (namestring path) mode sfinfo))

(defcfun ("sf_close" sf-close) :int
  (sndfile :pointer))

(defcfun ("sf_get_string" sf-get-string) :string
  (sndfile :pointer)
  (str_type :int))

(defcfun ("sf_set_string" sf-set-string) :int
  (sndfile :pointer)
  (str_type :int)
  (str :string))

(defcfun ("sf_version_string" sf-version-string) :string)

(defcfun ("sf_current_byterate" sf-current-bitrate) :int
  (sndfile :pointer))

;; Frame reading functions
;; ----------------------------------------------------------------------------

(defcfun ("sf_read_short" sf-read-short) :long
  (sndfile :pointer) (short :pointer) (items :long))

(defcfun ("sf_read_int" sf-read-int) :long
  (sndfile :pointer) (int :pointer) (items :long))

(defcfun ("sf_read_float" sf-read-float) :long
  (sndfile :pointer) (float :pointer) (items :long))

(defcfun ("sf_read_double" sf-read-double) :long
  (sndfile :pointer) (double :pointer) (items :long))

(defcfun ("sf_readf_short" sf-readf-short) :long
  (sndfile :pointer) (short :pointer) (frames :long))

(defcfun ("sf_readf_int" sf-readf-int) :long
  (sndfile :pointer) (int :pointer) (frames :long))

(defcfun ("sf_readf_float" sf-readf-float) :long
  (sndfile :pointer) (float :pointer) (frames :long))

(defcfun ("sf_readf_double" sf-readf-double) :long
  (sndfile :pointer) (double :pointer) (frames :long))

(defcfun ("sf_command" sf-command) :int
  (sndfile :pointer) (cmd :int) (data :pointer) (datasize :int))

;; ----------------------------------------------------------------------------
;; OVERLAY FUNCTIONS
;; ----------------------------------------------------------------------------

(defun audio-metadata (file-path)
  "Returns two values: A HANDLER for subsequent calls to libsndfile functions
and an assoc list containing metadata.  The handler should be closed with a
call to SF-CLOSE."

  (let ((metadata (foreign-alloc '(:struct SF_INFO))))
    (setf (foreign-slot-value metadata '(:struct SF_INFO) 'format) 0)
    (let* ((file-handle (sf-open file-path SFM_READ metadata))
           (output (make-instance 'track
                     :frames          (foreign-slot-value metadata '(:struct SF_INFO) 'frames)
                     :sample-rate     (foreign-slot-value metadata '(:struct SF_INFO) 'samplerate)
                     :channels        (foreign-slot-value metadata '(:struct SF_INFO) 'channels)
                     :format          (foreign-slot-value metadata '(:struct SF_INFO) 'format)
                     :sections        (foreign-slot-value metadata '(:struct SF_INFO) 'sections)
                     :seekable        (foreign-slot-value metadata '(:struct SF_INFO) 'seekable)
                     :initial-bitrate (sf-current-bitrate file-handle)
                     :title           (sf-get-string file-handle SF_STR_TITLE)
                     :copyright       (sf-get-string file-handle SF_STR_COPYRIGHT)
                     :software        (sf-get-string file-handle SF_STR_SOFTWARE)
                     :artist          (sf-get-string file-handle SF_STR_ARTIST)
                     :comment         (sf-get-string file-handle SF_STR_COMMENT)
                     :date            (sf-get-string file-handle SF_STR_DATE)
                     :album           (sf-get-string file-handle SF_STR_ALBUM)
                     :license         (sf-get-string file-handle SF_STR_LICENSE)
                     :tracknumber     (sf-get-string file-handle SF_STR_TRACKNUMBER)
                     :genre           (sf-get-string file-handle SF_STR_GENRE))))
      (foreign-free metadata)
      (values file-handle output))))

(defun broadcast-info (file-handle)
  "Return the Broadcast Extension Chunk in FILE-HANDLE if available."
  (with-foreign-object (info '(:struct SF_BROADCAST_INFO))
    (if (eq SF_TRUE (sf-command file-handle
                                 SFC_GET_BROADCAST_INFO
                                 info
                                 (foreign-type-size '(:struct SF_BROADCAST_INFO))))
        `((description          . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'description)
                                    :string))
          (originator           . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'originator)
                                    :string))
          (originator-reference . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'originator_reference)
                                    :string))
          (origination-date     . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'origination_date)
                                    :string))
          (origination-time     . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'origination_time)
                                    :string))
          (time-reference-low   . ,(foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'time_reference_low))
          (time-reference-high  . ,(foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'time_reference_high))
          (version              . ,(foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'version))
          (umid                 . ,(convert-from-foreign
                                    (foreign-slot-value info '(:struct SF_BROADCAST_INFO) 'umid)
                                    :string)))
        nil)))

(defun peak-value (file-handle)
  "Returns the peak value reported in the header of FILE-HANDLE."
  (let* ((max-peak       (foreign-alloc :double))
         (contains-value (sf-command file-handle
                                     SFC_GET_SIGNAL_MAX
                                     max-peak
                                     (foreign-type-size :double)))
         (output         (mem-ref max-peak :double)))
    (foreign-free max-peak)
    (if (eq contains-value SF_TRUE)
        output
        nil)))

(defun audio-format-details (format-code)
  "Return the audio format details."
  ;; This procedure still needs some work..
  (let ((info (foreign-alloc '(:struct SF_FORMAT_INFO))))
    (setf (foreign-slot-value info '(:struct SF_FORMAT_INFO) 'format)
          (convert-to-foreign format-code :int))
    (let* ((succesful-exec   (sf-command (null-pointer)
                                         SFC_GET_FORMAT_MAJOR
                                         info
                                         (foreign-type-size '(:struct SF_FORMAT_INFO))))
           (format-name      (foreign-slot-value info '(:struct SF_FORMAT_INFO) 'name))
           (format-extension (foreign-slot-value info '(:struct SF_FORMAT_INFO) 'extension)))
      (foreign-free info)
      (if (eq succesful-exec 0)
          `((format    . ,format-code)
            (name      . ,(foreign-string-to-lisp format-name))
            (extension . ,format-extension))
          succesful-exec))))
