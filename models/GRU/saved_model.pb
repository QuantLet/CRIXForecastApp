ÝŠ
Şý
8
Const
output"dtype"
valuetensor"
dtypetype

NoOp
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype
ž
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring 
q
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"serve*2.2.02v2.2.0-rc4-8-g2b96f3662b8
t
dense/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namedense/kernel
m
 dense/kernel/Read/ReadVariableOpReadVariableOpdense/kernel*
_output_shapes

: *
dtype0
l

dense/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_name
dense/bias
e
dense/bias/Read/ReadVariableOpReadVariableOp
dense/bias*
_output_shapes
:*
dtype0
l
RMSprop/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_nameRMSprop/iter
e
 RMSprop/iter/Read/ReadVariableOpReadVariableOpRMSprop/iter*
_output_shapes
: *
dtype0	
n
RMSprop/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/decay
g
!RMSprop/decay/Read/ReadVariableOpReadVariableOpRMSprop/decay*
_output_shapes
: *
dtype0
~
RMSprop/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *&
shared_nameRMSprop/learning_rate
w
)RMSprop/learning_rate/Read/ReadVariableOpReadVariableOpRMSprop/learning_rate*
_output_shapes
: *
dtype0
t
RMSprop/momentumVarHandleOp*
_output_shapes
: *
dtype0*
shape: *!
shared_nameRMSprop/momentum
m
$RMSprop/momentum/Read/ReadVariableOpReadVariableOpRMSprop/momentum*
_output_shapes
: *
dtype0
j
RMSprop/rhoVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/rho
c
RMSprop/rho/Read/ReadVariableOpReadVariableOpRMSprop/rho*
_output_shapes
: *
dtype0

gru/gru_cell/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*$
shared_namegru/gru_cell/kernel
{
'gru/gru_cell/kernel/Read/ReadVariableOpReadVariableOpgru/gru_cell/kernel*
_output_shapes

:`*
dtype0

gru/gru_cell/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*.
shared_namegru/gru_cell/recurrent_kernel

1gru/gru_cell/recurrent_kernel/Read/ReadVariableOpReadVariableOpgru/gru_cell/recurrent_kernel*
_output_shapes

: `*
dtype0
z
gru/gru_cell/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:`*"
shared_namegru/gru_cell/bias
s
%gru/gru_cell/bias/Read/ReadVariableOpReadVariableOpgru/gru_cell/bias*
_output_shapes
:`*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0

RMSprop/dense/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *)
shared_nameRMSprop/dense/kernel/rms

,RMSprop/dense/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense/kernel/rms*
_output_shapes

: *
dtype0

RMSprop/dense/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:*'
shared_nameRMSprop/dense/bias/rms
}
*RMSprop/dense/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense/bias/rms*
_output_shapes
:*
dtype0

RMSprop/gru/gru_cell/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*0
shared_name!RMSprop/gru/gru_cell/kernel/rms

3RMSprop/gru/gru_cell/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/gru/gru_cell/kernel/rms*
_output_shapes

:`*
dtype0
Ž
)RMSprop/gru/gru_cell/recurrent_kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*:
shared_name+)RMSprop/gru/gru_cell/recurrent_kernel/rms
§
=RMSprop/gru/gru_cell/recurrent_kernel/rms/Read/ReadVariableOpReadVariableOp)RMSprop/gru/gru_cell/recurrent_kernel/rms*
_output_shapes

: `*
dtype0

RMSprop/gru/gru_cell/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:`*.
shared_nameRMSprop/gru/gru_cell/bias/rms

1RMSprop/gru/gru_cell/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/gru/gru_cell/bias/rms*
_output_shapes
:`*
dtype0

NoOpNoOp

ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*Ę
valueŔB˝ Bś
ż
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
l
	cell


state_spec
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
v
iter
	decay
learning_rate
momentum
rho	rms;	rms<	rms=	rms>	rms?
#
0
1
2
3
4
#
0
1
2
3
4
 
­
metrics
non_trainable_variables

layers
 layer_regularization_losses
!layer_metrics
	variables
trainable_variables
regularization_losses
 
~

kernel
recurrent_kernel
bias
"	variables
#trainable_variables
$regularization_losses
%	keras_api
 

0
1
2

0
1
2
 
š
&metrics

'states
(non_trainable_variables

)layers
*layer_regularization_losses
+layer_metrics
	variables
trainable_variables
regularization_losses
XV
VARIABLE_VALUEdense/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE
TR
VARIABLE_VALUE
dense/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
­
,metrics
-non_trainable_variables

.layers
/layer_regularization_losses
0layer_metrics
	variables
trainable_variables
regularization_losses
KI
VARIABLE_VALUERMSprop/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
MK
VARIABLE_VALUERMSprop/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUERMSprop/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUERMSprop/momentum-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUE
IG
VARIABLE_VALUERMSprop/rho(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEgru/gru_cell/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE
YW
VARIABLE_VALUEgru/gru_cell/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE
MK
VARIABLE_VALUEgru/gru_cell/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE

10
 

0
1
 
 

0
1
2

0
1
2
 
­
2metrics
3non_trainable_variables

4layers
5layer_regularization_losses
6layer_metrics
"	variables
#trainable_variables
$regularization_losses
 
 
 

	0
 
 
 
 
 
 
 
4
	7total
	8count
9	variables
:	keras_api
 
 
 
 
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

70
81

9	variables

VARIABLE_VALUERMSprop/dense/kernel/rmsTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUERMSprop/dense/bias/rmsRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
yw
VARIABLE_VALUERMSprop/gru/gru_cell/kernel/rmsDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE)RMSprop/gru/gru_cell/recurrent_kernel/rmsDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
wu
VARIABLE_VALUERMSprop/gru/gru_cell/bias/rmsDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE

serving_default_gru_inputPlaceholder*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙*
dtype0*)
shape :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
ü
StatefulPartitionedCallStatefulPartitionedCallserving_default_gru_inputgru/gru_cell/kernelgru/gru_cell/biasgru/gru_cell/recurrent_kerneldense/kernel
dense/bias*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*,
f'R%
#__inference_signature_wrapper_16099
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
ź
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename dense/kernel/Read/ReadVariableOpdense/bias/Read/ReadVariableOp RMSprop/iter/Read/ReadVariableOp!RMSprop/decay/Read/ReadVariableOp)RMSprop/learning_rate/Read/ReadVariableOp$RMSprop/momentum/Read/ReadVariableOpRMSprop/rho/Read/ReadVariableOp'gru/gru_cell/kernel/Read/ReadVariableOp1gru/gru_cell/recurrent_kernel/Read/ReadVariableOp%gru/gru_cell/bias/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp,RMSprop/dense/kernel/rms/Read/ReadVariableOp*RMSprop/dense/bias/rms/Read/ReadVariableOp3RMSprop/gru/gru_cell/kernel/rms/Read/ReadVariableOp=RMSprop/gru/gru_cell/recurrent_kernel/rms/Read/ReadVariableOp1RMSprop/gru/gru_cell/bias/rms/Read/ReadVariableOpConst*
Tin
2	*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 **
config_proto

CPU

GPU 2J 8*'
f"R 
__inference__traced_save_17638
ă
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense/kernel
dense/biasRMSprop/iterRMSprop/decayRMSprop/learning_rateRMSprop/momentumRMSprop/rhogru/gru_cell/kernelgru/gru_cell/recurrent_kernelgru/gru_cell/biastotalcountRMSprop/dense/kernel/rmsRMSprop/dense/bias/rmsRMSprop/gru/gru_cell/kernel/rms)RMSprop/gru/gru_cell/recurrent_kernel/rmsRMSprop/gru/gru_cell/bias/rms*
Tin
2*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 **
config_proto

CPU

GPU 2J 8**
f%R#
!__inference__traced_restore_17701ěÂ


Ş
(__inference_gru_cell_layer_call_fn_17546

inputs
states_0
unknown
	unknown_0
	unknown_1
identity

identity_1˘StatefulPartitionedCallý
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150222
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ :::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:QM
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ł
ă
while_cond_15341
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_15341___redundant_placeholder0-
)while_cond_15341___redundant_placeholder1-
)while_cond_15341___redundant_placeholder2-
)while_cond_15341___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
×R
¨
while_body_17258
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
ŠT

sequential_gru_while_body_14812%
!sequential_gru_while_loop_counter+
'sequential_gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2$
 sequential_gru_strided_slice_1_0`
\tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4"
sequential_gru_strided_slice_1^
Ztensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeÄ
#TensorArrayV2Read/TensorListGetItemTensorListGetItem\tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/ym
add_1AddV2!sequential_gru_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identityn

Identity_1Identity'sequential_gru_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"B
sequential_gru_strided_slice_1 sequential_gru_strided_slice_1_0"ş
Ztensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor\tensorarrayv2read_tensorlistgetitem_sequential_gru_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
ł
ă
while_cond_17056
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_17056___redundant_placeholder0-
)while_cond_17056___redundant_placeholder1-
)while_cond_17056___redundant_placeholder2-
)while_cond_17056___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
äN
ˇ	
!__inference__traced_restore_17701
file_prefix!
assignvariableop_dense_kernel!
assignvariableop_1_dense_bias#
assignvariableop_2_rmsprop_iter$
 assignvariableop_3_rmsprop_decay,
(assignvariableop_4_rmsprop_learning_rate'
#assignvariableop_5_rmsprop_momentum"
assignvariableop_6_rmsprop_rho*
&assignvariableop_7_gru_gru_cell_kernel4
0assignvariableop_8_gru_gru_cell_recurrent_kernel(
$assignvariableop_9_gru_gru_cell_bias
assignvariableop_10_total
assignvariableop_11_count0
,assignvariableop_12_rmsprop_dense_kernel_rms.
*assignvariableop_13_rmsprop_dense_bias_rms7
3assignvariableop_14_rmsprop_gru_gru_cell_kernel_rmsA
=assignvariableop_15_rmsprop_gru_gru_cell_recurrent_kernel_rms5
1assignvariableop_16_rmsprop_gru_gru_cell_bias_rms
identity_18˘AssignVariableOp˘AssignVariableOp_1˘AssignVariableOp_10˘AssignVariableOp_11˘AssignVariableOp_12˘AssignVariableOp_13˘AssignVariableOp_14˘AssignVariableOp_15˘AssignVariableOp_16˘AssignVariableOp_2˘AssignVariableOp_3˘AssignVariableOp_4˘AssignVariableOp_5˘AssignVariableOp_6˘AssignVariableOp_7˘AssignVariableOp_8˘AssignVariableOp_9˘	RestoreV2˘RestoreV2_1ť
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*Ç
value˝BşB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE2
RestoreV2/tensor_names°
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*X
_output_shapesF
D:::::::::::::::::*
dtypes
2	2
	RestoreV2X
IdentityIdentityRestoreV2:tensors:0*
T0*
_output_shapes
:2

Identity
AssignVariableOpAssignVariableOpassignvariableop_dense_kernelIdentity:output:0*
_output_shapes
 *
dtype02
AssignVariableOp\

Identity_1IdentityRestoreV2:tensors:1*
T0*
_output_shapes
:2

Identity_1
AssignVariableOp_1AssignVariableOpassignvariableop_1_dense_biasIdentity_1:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_1\

Identity_2IdentityRestoreV2:tensors:2*
T0	*
_output_shapes
:2

Identity_2
AssignVariableOp_2AssignVariableOpassignvariableop_2_rmsprop_iterIdentity_2:output:0*
_output_shapes
 *
dtype0	2
AssignVariableOp_2\

Identity_3IdentityRestoreV2:tensors:3*
T0*
_output_shapes
:2

Identity_3
AssignVariableOp_3AssignVariableOp assignvariableop_3_rmsprop_decayIdentity_3:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_3\

Identity_4IdentityRestoreV2:tensors:4*
T0*
_output_shapes
:2

Identity_4
AssignVariableOp_4AssignVariableOp(assignvariableop_4_rmsprop_learning_rateIdentity_4:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_4\

Identity_5IdentityRestoreV2:tensors:5*
T0*
_output_shapes
:2

Identity_5
AssignVariableOp_5AssignVariableOp#assignvariableop_5_rmsprop_momentumIdentity_5:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_5\

Identity_6IdentityRestoreV2:tensors:6*
T0*
_output_shapes
:2

Identity_6
AssignVariableOp_6AssignVariableOpassignvariableop_6_rmsprop_rhoIdentity_6:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_6\

Identity_7IdentityRestoreV2:tensors:7*
T0*
_output_shapes
:2

Identity_7
AssignVariableOp_7AssignVariableOp&assignvariableop_7_gru_gru_cell_kernelIdentity_7:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_7\

Identity_8IdentityRestoreV2:tensors:8*
T0*
_output_shapes
:2

Identity_8Ś
AssignVariableOp_8AssignVariableOp0assignvariableop_8_gru_gru_cell_recurrent_kernelIdentity_8:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_8\

Identity_9IdentityRestoreV2:tensors:9*
T0*
_output_shapes
:2

Identity_9
AssignVariableOp_9AssignVariableOp$assignvariableop_9_gru_gru_cell_biasIdentity_9:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_9_
Identity_10IdentityRestoreV2:tensors:10*
T0*
_output_shapes
:2
Identity_10
AssignVariableOp_10AssignVariableOpassignvariableop_10_totalIdentity_10:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_10_
Identity_11IdentityRestoreV2:tensors:11*
T0*
_output_shapes
:2
Identity_11
AssignVariableOp_11AssignVariableOpassignvariableop_11_countIdentity_11:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_11_
Identity_12IdentityRestoreV2:tensors:12*
T0*
_output_shapes
:2
Identity_12Ľ
AssignVariableOp_12AssignVariableOp,assignvariableop_12_rmsprop_dense_kernel_rmsIdentity_12:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_12_
Identity_13IdentityRestoreV2:tensors:13*
T0*
_output_shapes
:2
Identity_13Ł
AssignVariableOp_13AssignVariableOp*assignvariableop_13_rmsprop_dense_bias_rmsIdentity_13:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_13_
Identity_14IdentityRestoreV2:tensors:14*
T0*
_output_shapes
:2
Identity_14Ź
AssignVariableOp_14AssignVariableOp3assignvariableop_14_rmsprop_gru_gru_cell_kernel_rmsIdentity_14:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_14_
Identity_15IdentityRestoreV2:tensors:15*
T0*
_output_shapes
:2
Identity_15ś
AssignVariableOp_15AssignVariableOp=assignvariableop_15_rmsprop_gru_gru_cell_recurrent_kernel_rmsIdentity_15:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_15_
Identity_16IdentityRestoreV2:tensors:16*
T0*
_output_shapes
:2
Identity_16Ş
AssignVariableOp_16AssignVariableOp1assignvariableop_16_rmsprop_gru_gru_cell_bias_rmsIdentity_16:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_16¨
RestoreV2_1/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*1
value(B&B_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2_1/tensor_names
RestoreV2_1/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*
valueB
B 2
RestoreV2_1/shape_and_slicesÄ
RestoreV2_1	RestoreV2file_prefix!RestoreV2_1/tensor_names:output:0%RestoreV2_1/shape_and_slices:output:0
^RestoreV2"/device:CPU:0*
_output_shapes
:*
dtypes
22
RestoreV2_19
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOpÔ
Identity_17Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_17á
Identity_18IdentityIdentity_17:output:0^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9
^RestoreV2^RestoreV2_1*
T0*
_output_shapes
: 2
Identity_18"#
identity_18Identity_18:output:0*Y
_input_shapesH
F: :::::::::::::::::2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162(
AssignVariableOp_2AssignVariableOp_22(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_92
	RestoreV2	RestoreV22
RestoreV2_1RestoreV2_1:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: :


_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
×R
¨
while_body_17057
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
S
Ä
gru_while_body_16189
gru_while_loop_counter 
gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
gru_strided_slice_1_0U
Qtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
gru_strided_slice_1S
Otensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeš
#TensorArrayV2Read/TensorListGetItemTensorListGetItemQtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/yb
add_1AddV2gru_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identityc

Identity_1Identitygru_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0",
gru_strided_slice_1gru_strided_slice_1_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"¤
Otensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensorQtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
÷
ô
E__inference_sequential_layer_call_and_return_conditional_losses_15995
	gru_input
	gru_15961
	gru_15963
	gru_15965
dense_15989
dense_15991
identity˘dense/StatefulPartitionedCall˘gru/StatefulPartitionedCallć
gru/StatefulPartitionedCallStatefulPartitionedCall	gru_input	gru_15961	gru_15963	gru_15965*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_157372
gru/StatefulPartitionedCallţ
dense/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0dense_15989dense_15991*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_dense_layer_call_and_return_conditional_losses_159782
dense/StatefulPartitionedCall¸
IdentityIdentity&dense/StatefulPartitionedCall:output:0^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ł
ă
while_cond_15625
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_15625___redundant_placeholder0-
)while_cond_15625___redundant_placeholder1-
)while_cond_15625___redundant_placeholder2-
)while_cond_15625___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ľ
Ň
E__inference_sequential_layer_call_and_return_conditional_losses_16513

inputs/
+gru_gru_cell_matmul_readvariableop_resource0
,gru_gru_cell_biasadd_readvariableop_resource(
$gru_gru_cell_readvariableop_resource(
$dense_matmul_readvariableop_resource)
%dense_biasadd_readvariableop_resource
identity˘	gru/whileL
	gru/ShapeShapeinputs*
T0*
_output_shapes
:2
	gru/Shape|
gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice/stack
gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_1
gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_2ú
gru/strided_sliceStridedSlicegru/Shape:output:0 gru/strided_slice/stack:output:0"gru/strided_slice/stack_1:output:0"gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_sliced
gru/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/zeros/mul/y|
gru/zeros/mulMulgru/strided_slice:output:0gru/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
gru/zeros/mulg
gru/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
gru/zeros/Less/yw
gru/zeros/LessLessgru/zeros/mul:z:0gru/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
gru/zeros/Lessj
gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru/zeros/packed/1
gru/zeros/packedPackgru/strided_slice:output:0gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru/zeros/packedg
gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/zeros/Const
	gru/zerosFillgru/zeros/packed:output:0gru/zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
	gru/zeros}
gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose/perm
gru/transpose	Transposeinputsgru/transpose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
gru/transpose[
gru/Shape_1Shapegru/transpose:y:0*
T0*
_output_shapes
:2
gru/Shape_1
gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_1/stack
gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_1
gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_2
gru/strided_slice_1StridedSlicegru/Shape_1:output:0"gru/strided_slice_1/stack:output:0$gru/strided_slice_1/stack_1:output:0$gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slice_1
gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2!
gru/TensorArrayV2/element_shapeÂ
gru/TensorArrayV2TensorListReserve(gru/TensorArrayV2/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2Ç
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   2;
9gru/TensorArrayUnstack/TensorListFromTensor/element_shape
+gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru/transpose:y:0Bgru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02-
+gru/TensorArrayUnstack/TensorListFromTensor
gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_2/stack
gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_1
gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_2
gru/strided_slice_2StridedSlicegru/transpose:y:0"gru/strided_slice_2/stack:output:0$gru/strided_slice_2/stack_1:output:0$gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
gru/strided_slice_2´
"gru/gru_cell/MatMul/ReadVariableOpReadVariableOp+gru_gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02$
"gru/gru_cell/MatMul/ReadVariableOp°
gru/gru_cell/MatMulMatMulgru/strided_slice_2:output:0*gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru/gru_cell/MatMulł
#gru/gru_cell/BiasAdd/ReadVariableOpReadVariableOp,gru_gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02%
#gru/gru_cell/BiasAdd/ReadVariableOpľ
gru/gru_cell/BiasAddBiasAddgru/gru_cell/MatMul:product:0+gru/gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru/gru_cell/BiasAddj
gru/gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru/gru_cell/Const
gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru/gru_cell/split/split_dimŕ
gru/gru_cell/splitSplit%gru/gru_cell/split/split_dim:output:0gru/gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru/gru_cell/split
gru/gru_cell/ReadVariableOpReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru/gru_cell/ReadVariableOp
 gru/gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru/gru_cell/strided_slice/stack
"gru/gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru/gru_cell/strided_slice/stack_1
"gru/gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru/gru_cell/strided_slice/stack_2Ę
gru/gru_cell/strided_sliceStridedSlice#gru/gru_cell/ReadVariableOp:value:0)gru/gru_cell/strided_slice/stack:output:0+gru/gru_cell/strided_slice/stack_1:output:0+gru/gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru/gru_cell/strided_sliceŁ
gru/gru_cell/MatMul_1MatMulgru/zeros:output:0#gru/gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru/gru_cell/MatMul_1
gru/gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru/gru_cell/Const_1
gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2 
gru/gru_cell/split_1/split_dim
gru/gru_cell/split_1SplitVgru/gru_cell/MatMul_1:product:0gru/gru_cell/Const_1:output:0'gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru/gru_cell/split_1
gru/gru_cell/addAddV2gru/gru_cell/split:output:0gru/gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/addq
gru/gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru/gru_cell/Const_2q
gru/gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru/gru_cell/Const_3
gru/gru_cell/MulMulgru/gru_cell/add:z:0gru/gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Mul
gru/gru_cell/Add_1Addgru/gru_cell/Mul:z:0gru/gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Add_1
$gru/gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$gru/gru_cell/clip_by_value/Minimum/yĚ
"gru/gru_cell/clip_by_value/MinimumMinimumgru/gru_cell/Add_1:z:0-gru/gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2$
"gru/gru_cell/clip_by_value/Minimum
gru/gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/gru_cell/clip_by_value/yÄ
gru/gru_cell/clip_by_valueMaximum&gru/gru_cell/clip_by_value/Minimum:z:0%gru/gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/clip_by_value
gru/gru_cell/add_2AddV2gru/gru_cell/split:output:1gru/gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_2q
gru/gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru/gru_cell/Const_4q
gru/gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru/gru_cell/Const_5
gru/gru_cell/Mul_1Mulgru/gru_cell/add_2:z:0gru/gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Mul_1
gru/gru_cell/Add_3Addgru/gru_cell/Mul_1:z:0gru/gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Add_3
&gru/gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&gru/gru_cell/clip_by_value_1/Minimum/yŇ
$gru/gru_cell/clip_by_value_1/MinimumMinimumgru/gru_cell/Add_3:z:0/gru/gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2&
$gru/gru_cell/clip_by_value_1/Minimum
gru/gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
gru/gru_cell/clip_by_value_1/yĚ
gru/gru_cell/clip_by_value_1Maximum(gru/gru_cell/clip_by_value_1/Minimum:z:0'gru/gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/clip_by_value_1
gru/gru_cell/mul_2Mul gru/gru_cell/clip_by_value_1:z:0gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_2Ł
gru/gru_cell/ReadVariableOp_1ReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru/gru_cell/ReadVariableOp_1
"gru/gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru/gru_cell/strided_slice_1/stack
$gru/gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2&
$gru/gru_cell/strided_slice_1/stack_1
$gru/gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2&
$gru/gru_cell/strided_slice_1/stack_2Ö
gru/gru_cell/strided_slice_1StridedSlice%gru/gru_cell/ReadVariableOp_1:value:0+gru/gru_cell/strided_slice_1/stack:output:0-gru/gru_cell/strided_slice_1/stack_1:output:0-gru/gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru/gru_cell/strided_slice_1Š
gru/gru_cell/MatMul_2MatMulgru/gru_cell/mul_2:z:0%gru/gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/MatMul_2Ą
gru/gru_cell/add_4AddV2gru/gru_cell/split:output:2gru/gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_4x
gru/gru_cell/TanhTanhgru/gru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Tanh
gru/gru_cell/mul_3Mulgru/gru_cell/clip_by_value:z:0gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_3m
gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru/gru_cell/sub/x
gru/gru_cell/subSubgru/gru_cell/sub/x:output:0gru/gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/sub
gru/gru_cell/mul_4Mulgru/gru_cell/sub:z:0gru/gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_4
gru/gru_cell/add_5AddV2gru/gru_cell/mul_3:z:0gru/gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_5
!gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2#
!gru/TensorArrayV2_1/element_shapeČ
gru/TensorArrayV2_1TensorListReserve*gru/TensorArrayV2_1/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2_1V
gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru/time
gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru/while/maximum_iterationsr
gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/while/loop_counterÖ
	gru/whileWhilegru/while/loop_counter:output:0%gru/while/maximum_iterations:output:0gru/time:output:0gru/TensorArrayV2_1:handle:0gru/zeros:output:0gru/strided_slice_1:output:0;gru/TensorArrayUnstack/TensorListFromTensor:output_handle:0+gru_gru_cell_matmul_readvariableop_resource,gru_gru_cell_biasadd_readvariableop_resource$gru_gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	* 
bodyR
gru_while_body_16396* 
condR
gru_while_cond_16395*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
	gru/while˝
4gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    26
4gru/TensorArrayV2Stack/TensorListStack/element_shape
&gru/TensorArrayV2Stack/TensorListStackTensorListStackgru/while:output:3=gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02(
&gru/TensorArrayV2Stack/TensorListStack
gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
gru/strided_slice_3/stack
gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_3/stack_1
gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_3/stack_2˛
gru/strided_slice_3StridedSlice/gru/TensorArrayV2Stack/TensorListStack:tensor:0"gru/strided_slice_3/stack:output:0$gru/strided_slice_3/stack_1:output:0$gru/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
gru/strided_slice_3
gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose_1/permž
gru/transpose_1	Transpose/gru/TensorArrayV2Stack/TensorListStack:tensor:0gru/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
gru/transpose_1n
gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/runtime
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense/MatMul/ReadVariableOp
dense/MatMulMatMulgru/strided_slice_3:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
dense/MatMul
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02
dense/BiasAdd/ReadVariableOp
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
dense/BiasAddv
IdentityIdentitydense/BiasAdd:output:0
^gru/while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2
	gru/while	gru/while:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
É

#__inference_gru_layer_call_fn_17391
inputs_0
unknown
	unknown_0
	unknown_1
identity˘StatefulPartitionedCallŰ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_155242
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
är
č
>__inference_gru_layer_call_and_return_conditional_losses_17168
inputs_0+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_17057*
condR
while_cond_17056*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:^ Z
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
×R
¨
while_body_15827
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 

Á
while_body_15460
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
gru_cell_15482_0
gru_cell_15484_0
gru_cell_15486_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
gru_cell_15482
gru_cell_15484
gru_cell_15486˘ gru_cell/StatefulPartitionedCallˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemĎ
 gru_cell/StatefulPartitionedCallStatefulPartitionedCall*TensorArrayV2Read/TensorListGetItem:item:0placeholder_2gru_cell_15482_0gru_cell_15484_0gru_cell_15486_0*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150832"
 gru_cell/StatefulPartitionedCallŐ
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholder)gru_cell/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1o
IdentityIdentity	add_1:z:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity

Identity_1Identitywhile_maximum_iterations!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_1q

Identity_2Identityadd:z:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_2

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_3¤

Identity_4Identity)gru_cell/StatefulPartitionedCall:output:1!^gru_cell/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4""
gru_cell_15482gru_cell_15482_0""
gru_cell_15484gru_cell_15484_0""
gru_cell_15486gru_cell_15486_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
ă4
ä
C__inference_gru_cell_layer_call_and_return_conditional_losses_15083

inputs

states"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
readvariableop_resource
identity

identity_1
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:`*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split/split_dimŹ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
splitx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ü
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
strided_slicep
MatMul_1MatMulstatesstrided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2

MatMul_1g
Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2	
Const_1q
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split_1/split_dimŃ
split_1SplitVMatMul_1:product:0Const_1:output:0split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
addW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3^
MulMuladd:z:0Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mulb
Add_1AddMul:z:0Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value/Minimum/y
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_valuek
add_2AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_2W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5d
Mul_1Mul	add_2:z:0Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mul_1d
Add_3Add	Mul_1:z:0Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value_1/Minimum/y
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1d
mul_2Mulclip_by_value_1:z:0states*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_2|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stack_1
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_1u
MatMul_2MatMul	mul_2:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

MatMul_2m
add_4AddV2split:output:2MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Tanhb
mul_3Mulclip_by_value:z:0states*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_3S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
subZ
mul_4Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_4_
add_5AddV2	mul_3:z:0	mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ ::::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:OK
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
 
_user_specified_namestates:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
í4
ć
C__inference_gru_cell_layer_call_and_return_conditional_losses_17532

inputs
states_0"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
readvariableop_resource
identity

identity_1
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:`*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split/split_dimŹ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
splitx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ü
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
strided_slicer
MatMul_1MatMulstates_0strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2

MatMul_1g
Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2	
Const_1q
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split_1/split_dimŃ
split_1SplitVMatMul_1:product:0Const_1:output:0split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
addW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3^
MulMuladd:z:0Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mulb
Add_1AddMul:z:0Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value/Minimum/y
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_valuek
add_2AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_2W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5d
Mul_1Mul	add_2:z:0Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mul_1d
Add_3Add	Mul_1:z:0Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value_1/Minimum/y
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1f
mul_2Mulclip_by_value_1:z:0states_0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_2|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stack_1
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_1u
MatMul_2MatMul	mul_2:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

MatMul_2m
add_4AddV2split:output:2MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Tanhd
mul_3Mulclip_by_value:z:0states_0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_3S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
subZ
mul_4Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_4_
add_5AddV2	mul_3:z:0	mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ ::::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:QM
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ţ<
Ä
>__inference_gru_layer_call_and_return_conditional_losses_15524

inputs
gru_cell_15448
gru_cell_15450
gru_cell_15452
identity˘ gru_cell/StatefulPartitionedCall˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¸
 gru_cell/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_15448gru_cell_15450gru_cell_15452*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150832"
 gru_cell/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterŮ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_15448gru_cell_15450gru_cell_15452*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_15460*
condR
while_cond_15459*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime
IdentityIdentitystrided_slice_3:output:0!^gru_cell/StatefulPartitionedCall^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
×R
¨
while_body_16633
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 

Á
while_body_15342
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
gru_cell_15364_0
gru_cell_15366_0
gru_cell_15368_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
gru_cell_15364
gru_cell_15366
gru_cell_15368˘ gru_cell/StatefulPartitionedCallˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemĎ
 gru_cell/StatefulPartitionedCallStatefulPartitionedCall*TensorArrayV2Read/TensorListGetItem:item:0placeholder_2gru_cell_15364_0gru_cell_15366_0gru_cell_15368_0*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150222"
 gru_cell/StatefulPartitionedCallŐ
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholder)gru_cell/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1o
IdentityIdentity	add_1:z:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity

Identity_1Identitywhile_maximum_iterations!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_1q

Identity_2Identityadd:z:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_2

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0!^gru_cell/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_3¤

Identity_4Identity)gru_cell/StatefulPartitionedCall:output:1!^gru_cell/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4""
gru_cell_15364gru_cell_15364_0""
gru_cell_15366gru_cell_15366_0""
gru_cell_15368gru_cell_15368_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
×

gru_while_cond_16395
gru_while_loop_counter 
gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_gru_strided_slice_11
-gru_while_cond_16395___redundant_placeholder01
-gru_while_cond_16395___redundant_placeholder11
-gru_while_cond_16395___redundant_placeholder21
-gru_while_cond_16395___redundant_placeholder3
identity
\
LessLessplaceholderless_gru_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
É

#__inference_gru_layer_call_fn_17380
inputs_0
unknown
	unknown_0
	unknown_1
identity˘StatefulPartitionedCallŰ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_154062
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ă

#__inference_gru_layer_call_fn_16956

inputs
unknown
	unknown_0
	unknown_1
identity˘StatefulPartitionedCallŮ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_157372
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 

¨
@__inference_dense_layer_call_and_return_conditional_losses_17401

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2	
BiasAddd
IdentityIdentityBiasAdd:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*.
_input_shapes
:˙˙˙˙˙˙˙˙˙ :::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
ł
ă
while_cond_16632
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_16632___redundant_placeholder0-
)while_cond_16632___redundant_placeholder1-
)while_cond_16632___redundant_placeholder2-
)while_cond_16632___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ür
ć
>__inference_gru_layer_call_and_return_conditional_losses_15938

inputs+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_15827*
condR
while_cond_15826*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ł
ă
while_cond_16833
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_16833___redundant_placeholder0-
)while_cond_16833___redundant_placeholder1-
)while_cond_16833___redundant_placeholder2-
)while_cond_16833___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
×

gru_while_cond_16188
gru_while_loop_counter 
gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_gru_strided_slice_11
-gru_while_cond_16188___redundant_placeholder01
-gru_while_cond_16188___redundant_placeholder11
-gru_while_cond_16188___redundant_placeholder21
-gru_while_cond_16188___redundant_placeholder3
identity
\
LessLessplaceholderless_gru_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
×R
¨
while_body_15626
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
Ür
ć
>__inference_gru_layer_call_and_return_conditional_losses_16945

inputs+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_16834*
condR
while_cond_16833*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
×R
¨
while_body_16834
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeľ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 

¨
@__inference_dense_layer_call_and_return_conditional_losses_15978

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2	
BiasAddd
IdentityIdentityBiasAdd:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*.
_input_shapes
:˙˙˙˙˙˙˙˙˙ :::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
S
Ä
gru_while_body_16396
gru_while_loop_counter 
gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
gru_strided_slice_1_0U
Qtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0-
)gru_cell_matmul_readvariableop_resource_0.
*gru_cell_biasadd_readvariableop_resource_0&
"gru_cell_readvariableop_resource_0
identity

identity_1

identity_2

identity_3

identity_4
gru_strided_slice_1S
Otensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resourceˇ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   23
1TensorArrayV2Read/TensorListGetItem/element_shapeš
#TensorArrayV2Read/TensorListGetItemTensorListGetItemQtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemŞ
gru_cell/MatMul/ReadVariableOpReadVariableOp)gru_cell_matmul_readvariableop_resource_0*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp˛
gru_cell/MatMulMatMul*TensorArrayV2Read/TensorListGetItem:item:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMulŠ
gru_cell/BiasAdd/ReadVariableOpReadVariableOp*gru_cell_biasadd_readvariableop_resource_0*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulplaceholder_2gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp"gru_cell_readvariableop_resource_0*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5ž
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/yb
add_1AddV2gru_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identityc

Identity_1Identitygru_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3j

Identity_4Identitygru_cell/add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_4"V
(gru_cell_biasadd_readvariableop_resource*gru_cell_biasadd_readvariableop_resource_0"T
'gru_cell_matmul_readvariableop_resource)gru_cell_matmul_readvariableop_resource_0"F
 gru_cell_readvariableop_resource"gru_cell_readvariableop_resource_0",
gru_strided_slice_1gru_strided_slice_1_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"¤
Otensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensorQtensorarrayv2read_tensorlistgetitem_gru_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :˙˙˙˙˙˙˙˙˙ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
ł
ă
while_cond_15826
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_15826___redundant_placeholder0-
)while_cond_15826___redundant_placeholder1-
)while_cond_15826___redundant_placeholder2-
)while_cond_15826___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ö
Ż
*__inference_sequential_layer_call_fn_16074
	gru_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identity˘StatefulPartitionedCallý
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_sequential_layer_call_and_return_conditional_losses_160612
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::22
StatefulPartitionedCallStatefulPartitionedCall:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ş
Ű
sequential_gru_while_cond_14811%
!sequential_gru_while_loop_counter+
'sequential_gru_while_maximum_iterations
placeholder
placeholder_1
placeholder_2'
#less_sequential_gru_strided_slice_1<
8sequential_gru_while_cond_14811___redundant_placeholder0<
8sequential_gru_while_cond_14811___redundant_placeholder1<
8sequential_gru_while_cond_14811___redundant_placeholder2<
8sequential_gru_while_cond_14811___redundant_placeholder3
identity
g
LessLessplaceholder#less_sequential_gru_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ö
Ż
*__inference_sequential_layer_call_fn_16043
	gru_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identity˘StatefulPartitionedCallý
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_sequential_layer_call_and_return_conditional_losses_160302
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::22
StatefulPartitionedCallStatefulPartitionedCall:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ă

#__inference_gru_layer_call_fn_16967

inputs
unknown
	unknown_0
	unknown_1
identity˘StatefulPartitionedCallŮ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_159382
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Í
Ź
*__inference_sequential_layer_call_fn_16543

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identity˘StatefulPartitionedCallú
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_sequential_layer_call_and_return_conditional_losses_160612
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ş
¨
#__inference_signature_wrapper_16099
	gru_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identity˘StatefulPartitionedCallŘ
StatefulPartitionedCallStatefulPartitionedCall	gru_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*)
f$R"
 __inference__wrapped_model_149292
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::22
StatefulPartitionedCallStatefulPartitionedCall:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ł
ă
while_cond_15459
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_15459___redundant_placeholder0-
)while_cond_15459___redundant_placeholder1-
)while_cond_15459___redundant_placeholder2-
)while_cond_15459___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
ł
ă
while_cond_17257
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_17257___redundant_placeholder0-
)while_cond_17257___redundant_placeholder1-
)while_cond_17257___redundant_placeholder2-
)while_cond_17257___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :˙˙˙˙˙˙˙˙˙ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ľ
Ň
E__inference_sequential_layer_call_and_return_conditional_losses_16306

inputs/
+gru_gru_cell_matmul_readvariableop_resource0
,gru_gru_cell_biasadd_readvariableop_resource(
$gru_gru_cell_readvariableop_resource(
$dense_matmul_readvariableop_resource)
%dense_biasadd_readvariableop_resource
identity˘	gru/whileL
	gru/ShapeShapeinputs*
T0*
_output_shapes
:2
	gru/Shape|
gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice/stack
gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_1
gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice/stack_2ú
gru/strided_sliceStridedSlicegru/Shape:output:0 gru/strided_slice/stack:output:0"gru/strided_slice/stack_1:output:0"gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_sliced
gru/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/zeros/mul/y|
gru/zeros/mulMulgru/strided_slice:output:0gru/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
gru/zeros/mulg
gru/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
gru/zeros/Less/yw
gru/zeros/LessLessgru/zeros/mul:z:0gru/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
gru/zeros/Lessj
gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru/zeros/packed/1
gru/zeros/packedPackgru/strided_slice:output:0gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru/zeros/packedg
gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/zeros/Const
	gru/zerosFillgru/zeros/packed:output:0gru/zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
	gru/zeros}
gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose/perm
gru/transpose	Transposeinputsgru/transpose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
gru/transpose[
gru/Shape_1Shapegru/transpose:y:0*
T0*
_output_shapes
:2
gru/Shape_1
gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_1/stack
gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_1
gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_1/stack_2
gru/strided_slice_1StridedSlicegru/Shape_1:output:0"gru/strided_slice_1/stack:output:0$gru/strided_slice_1/stack_1:output:0$gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru/strided_slice_1
gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2!
gru/TensorArrayV2/element_shapeÂ
gru/TensorArrayV2TensorListReserve(gru/TensorArrayV2/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2Ç
9gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   2;
9gru/TensorArrayUnstack/TensorListFromTensor/element_shape
+gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru/transpose:y:0Bgru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02-
+gru/TensorArrayUnstack/TensorListFromTensor
gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_2/stack
gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_1
gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_2/stack_2
gru/strided_slice_2StridedSlicegru/transpose:y:0"gru/strided_slice_2/stack:output:0$gru/strided_slice_2/stack_1:output:0$gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
gru/strided_slice_2´
"gru/gru_cell/MatMul/ReadVariableOpReadVariableOp+gru_gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02$
"gru/gru_cell/MatMul/ReadVariableOp°
gru/gru_cell/MatMulMatMulgru/strided_slice_2:output:0*gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru/gru_cell/MatMulł
#gru/gru_cell/BiasAdd/ReadVariableOpReadVariableOp,gru_gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02%
#gru/gru_cell/BiasAdd/ReadVariableOpľ
gru/gru_cell/BiasAddBiasAddgru/gru_cell/MatMul:product:0+gru/gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru/gru_cell/BiasAddj
gru/gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru/gru_cell/Const
gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru/gru_cell/split/split_dimŕ
gru/gru_cell/splitSplit%gru/gru_cell/split/split_dim:output:0gru/gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru/gru_cell/split
gru/gru_cell/ReadVariableOpReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru/gru_cell/ReadVariableOp
 gru/gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru/gru_cell/strided_slice/stack
"gru/gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru/gru_cell/strided_slice/stack_1
"gru/gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru/gru_cell/strided_slice/stack_2Ę
gru/gru_cell/strided_sliceStridedSlice#gru/gru_cell/ReadVariableOp:value:0)gru/gru_cell/strided_slice/stack:output:0+gru/gru_cell/strided_slice/stack_1:output:0+gru/gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru/gru_cell/strided_sliceŁ
gru/gru_cell/MatMul_1MatMulgru/zeros:output:0#gru/gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru/gru_cell/MatMul_1
gru/gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru/gru_cell/Const_1
gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2 
gru/gru_cell/split_1/split_dim
gru/gru_cell/split_1SplitVgru/gru_cell/MatMul_1:product:0gru/gru_cell/Const_1:output:0'gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru/gru_cell/split_1
gru/gru_cell/addAddV2gru/gru_cell/split:output:0gru/gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/addq
gru/gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru/gru_cell/Const_2q
gru/gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru/gru_cell/Const_3
gru/gru_cell/MulMulgru/gru_cell/add:z:0gru/gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Mul
gru/gru_cell/Add_1Addgru/gru_cell/Mul:z:0gru/gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Add_1
$gru/gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$gru/gru_cell/clip_by_value/Minimum/yĚ
"gru/gru_cell/clip_by_value/MinimumMinimumgru/gru_cell/Add_1:z:0-gru/gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2$
"gru/gru_cell/clip_by_value/Minimum
gru/gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/gru_cell/clip_by_value/yÄ
gru/gru_cell/clip_by_valueMaximum&gru/gru_cell/clip_by_value/Minimum:z:0%gru/gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/clip_by_value
gru/gru_cell/add_2AddV2gru/gru_cell/split:output:1gru/gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_2q
gru/gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru/gru_cell/Const_4q
gru/gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru/gru_cell/Const_5
gru/gru_cell/Mul_1Mulgru/gru_cell/add_2:z:0gru/gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Mul_1
gru/gru_cell/Add_3Addgru/gru_cell/Mul_1:z:0gru/gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Add_3
&gru/gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&gru/gru_cell/clip_by_value_1/Minimum/yŇ
$gru/gru_cell/clip_by_value_1/MinimumMinimumgru/gru_cell/Add_3:z:0/gru/gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2&
$gru/gru_cell/clip_by_value_1/Minimum
gru/gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
gru/gru_cell/clip_by_value_1/yĚ
gru/gru_cell/clip_by_value_1Maximum(gru/gru_cell/clip_by_value_1/Minimum:z:0'gru/gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/clip_by_value_1
gru/gru_cell/mul_2Mul gru/gru_cell/clip_by_value_1:z:0gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_2Ł
gru/gru_cell/ReadVariableOp_1ReadVariableOp$gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru/gru_cell/ReadVariableOp_1
"gru/gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru/gru_cell/strided_slice_1/stack
$gru/gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2&
$gru/gru_cell/strided_slice_1/stack_1
$gru/gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2&
$gru/gru_cell/strided_slice_1/stack_2Ö
gru/gru_cell/strided_slice_1StridedSlice%gru/gru_cell/ReadVariableOp_1:value:0+gru/gru_cell/strided_slice_1/stack:output:0-gru/gru_cell/strided_slice_1/stack_1:output:0-gru/gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru/gru_cell/strided_slice_1Š
gru/gru_cell/MatMul_2MatMulgru/gru_cell/mul_2:z:0%gru/gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/MatMul_2Ą
gru/gru_cell/add_4AddV2gru/gru_cell/split:output:2gru/gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_4x
gru/gru_cell/TanhTanhgru/gru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/Tanh
gru/gru_cell/mul_3Mulgru/gru_cell/clip_by_value:z:0gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_3m
gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru/gru_cell/sub/x
gru/gru_cell/subSubgru/gru_cell/sub/x:output:0gru/gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/sub
gru/gru_cell/mul_4Mulgru/gru_cell/sub:z:0gru/gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/mul_4
gru/gru_cell/add_5AddV2gru/gru_cell/mul_3:z:0gru/gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru/gru_cell/add_5
!gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2#
!gru/TensorArrayV2_1/element_shapeČ
gru/TensorArrayV2_1TensorListReserve*gru/TensorArrayV2_1/element_shape:output:0gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru/TensorArrayV2_1V
gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru/time
gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru/while/maximum_iterationsr
gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru/while/loop_counterÖ
	gru/whileWhilegru/while/loop_counter:output:0%gru/while/maximum_iterations:output:0gru/time:output:0gru/TensorArrayV2_1:handle:0gru/zeros:output:0gru/strided_slice_1:output:0;gru/TensorArrayUnstack/TensorListFromTensor:output_handle:0+gru_gru_cell_matmul_readvariableop_resource,gru_gru_cell_biasadd_readvariableop_resource$gru_gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	* 
bodyR
gru_while_body_16189* 
condR
gru_while_cond_16188*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
	gru/while˝
4gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    26
4gru/TensorArrayV2Stack/TensorListStack/element_shape
&gru/TensorArrayV2Stack/TensorListStackTensorListStackgru/while:output:3=gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02(
&gru/TensorArrayV2Stack/TensorListStack
gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
gru/strided_slice_3/stack
gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru/strided_slice_3/stack_1
gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru/strided_slice_3/stack_2˛
gru/strided_slice_3StridedSlice/gru/TensorArrayV2Stack/TensorListStack:tensor:0"gru/strided_slice_3/stack:output:0$gru/strided_slice_3/stack_1:output:0$gru/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
gru/strided_slice_3
gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru/transpose_1/permž
gru/transpose_1	Transpose/gru/TensorArrayV2Stack/TensorListStack:tensor:0gru/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
gru/transpose_1n
gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru/runtime
dense/MatMul/ReadVariableOpReadVariableOp$dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense/MatMul/ReadVariableOp
dense/MatMulMatMulgru/strided_slice_3:output:0#dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
dense/MatMul
dense/BiasAdd/ReadVariableOpReadVariableOp%dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02
dense/BiasAdd/ReadVariableOp
dense/BiasAddBiasAdddense/MatMul:product:0$dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
dense/BiasAddv
IdentityIdentitydense/BiasAdd:output:0
^gru/while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2
	gru/while	gru/while:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ür
ć
>__inference_gru_layer_call_and_return_conditional_losses_16744

inputs+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_16633*
condR
while_cond_16632*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
î
z
%__inference_dense_layer_call_fn_17410

inputs
unknown
	unknown_0
identity˘StatefulPartitionedCallÎ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_dense_layer_call_and_return_conditional_losses_159782
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*.
_input_shapes
:˙˙˙˙˙˙˙˙˙ ::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
î
ń
E__inference_sequential_layer_call_and_return_conditional_losses_16061

inputs
	gru_16048
	gru_16050
	gru_16052
dense_16055
dense_16057
identity˘dense/StatefulPartitionedCall˘gru/StatefulPartitionedCallă
gru/StatefulPartitionedCallStatefulPartitionedCallinputs	gru_16048	gru_16050	gru_16052*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_159382
gru/StatefulPartitionedCallţ
dense/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0dense_16055dense_16057*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_dense_layer_call_and_return_conditional_losses_159782
dense/StatefulPartitionedCall¸
IdentityIdentity&dense/StatefulPartitionedCall:output:0^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 

ň
 __inference__wrapped_model_14929
	gru_input:
6sequential_gru_gru_cell_matmul_readvariableop_resource;
7sequential_gru_gru_cell_biasadd_readvariableop_resource3
/sequential_gru_gru_cell_readvariableop_resource3
/sequential_dense_matmul_readvariableop_resource4
0sequential_dense_biasadd_readvariableop_resource
identity˘sequential/gru/whilee
sequential/gru/ShapeShape	gru_input*
T0*
_output_shapes
:2
sequential/gru/Shape
"sequential/gru/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2$
"sequential/gru/strided_slice/stack
$sequential/gru/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2&
$sequential/gru/strided_slice/stack_1
$sequential/gru/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2&
$sequential/gru/strided_slice/stack_2ź
sequential/gru/strided_sliceStridedSlicesequential/gru/Shape:output:0+sequential/gru/strided_slice/stack:output:0-sequential/gru/strided_slice/stack_1:output:0-sequential/gru/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
sequential/gru/strided_slicez
sequential/gru/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential/gru/zeros/mul/y¨
sequential/gru/zeros/mulMul%sequential/gru/strided_slice:output:0#sequential/gru/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
sequential/gru/zeros/mul}
sequential/gru/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
sequential/gru/zeros/Less/yŁ
sequential/gru/zeros/LessLesssequential/gru/zeros/mul:z:0$sequential/gru/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
sequential/gru/zeros/Less
sequential/gru/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
sequential/gru/zeros/packed/1ż
sequential/gru/zeros/packedPack%sequential/gru/strided_slice:output:0&sequential/gru/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
sequential/gru/zeros/packed}
sequential/gru/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru/zeros/Constą
sequential/gru/zerosFill$sequential/gru/zeros/packed:output:0#sequential/gru/zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/zeros
sequential/gru/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
sequential/gru/transpose/permł
sequential/gru/transpose	Transpose	gru_input&sequential/gru/transpose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
sequential/gru/transpose|
sequential/gru/Shape_1Shapesequential/gru/transpose:y:0*
T0*
_output_shapes
:2
sequential/gru/Shape_1
$sequential/gru/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2&
$sequential/gru/strided_slice_1/stack
&sequential/gru/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_1/stack_1
&sequential/gru/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_1/stack_2Č
sequential/gru/strided_slice_1StridedSlicesequential/gru/Shape_1:output:0-sequential/gru/strided_slice_1/stack:output:0/sequential/gru/strided_slice_1/stack_1:output:0/sequential/gru/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2 
sequential/gru/strided_slice_1Ł
*sequential/gru/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2,
*sequential/gru/TensorArrayV2/element_shapeî
sequential/gru/TensorArrayV2TensorListReserve3sequential/gru/TensorArrayV2/element_shape:output:0'sequential/gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
sequential/gru/TensorArrayV2Ý
Dsequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   2F
Dsequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shape´
6sequential/gru/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorsequential/gru/transpose:y:0Msequential/gru/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type028
6sequential/gru/TensorArrayUnstack/TensorListFromTensor
$sequential/gru/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2&
$sequential/gru/strided_slice_2/stack
&sequential/gru/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_2/stack_1
&sequential/gru/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_2/stack_2Ö
sequential/gru/strided_slice_2StridedSlicesequential/gru/transpose:y:0-sequential/gru/strided_slice_2/stack:output:0/sequential/gru/strided_slice_2/stack_1:output:0/sequential/gru/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2 
sequential/gru/strided_slice_2Ő
-sequential/gru/gru_cell/MatMul/ReadVariableOpReadVariableOp6sequential_gru_gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02/
-sequential/gru/gru_cell/MatMul/ReadVariableOpÜ
sequential/gru/gru_cell/MatMulMatMul'sequential/gru/strided_slice_2:output:05sequential/gru/gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2 
sequential/gru/gru_cell/MatMulÔ
.sequential/gru/gru_cell/BiasAdd/ReadVariableOpReadVariableOp7sequential_gru_gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype020
.sequential/gru/gru_cell/BiasAdd/ReadVariableOpá
sequential/gru/gru_cell/BiasAddBiasAdd(sequential/gru/gru_cell/MatMul:product:06sequential/gru/gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2!
sequential/gru/gru_cell/BiasAdd
sequential/gru/gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
sequential/gru/gru_cell/Const
'sequential/gru/gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2)
'sequential/gru/gru_cell/split/split_dim
sequential/gru/gru_cell/splitSplit0sequential/gru/gru_cell/split/split_dim:output:0(sequential/gru/gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
sequential/gru/gru_cell/splitŔ
&sequential/gru/gru_cell/ReadVariableOpReadVariableOp/sequential_gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02(
&sequential/gru/gru_cell/ReadVariableOpŤ
+sequential/gru/gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2-
+sequential/gru/gru_cell/strided_slice/stackŻ
-sequential/gru/gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2/
-sequential/gru/gru_cell/strided_slice/stack_1Ż
-sequential/gru/gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2/
-sequential/gru/gru_cell/strided_slice/stack_2
%sequential/gru/gru_cell/strided_sliceStridedSlice.sequential/gru/gru_cell/ReadVariableOp:value:04sequential/gru/gru_cell/strided_slice/stack:output:06sequential/gru/gru_cell/strided_slice/stack_1:output:06sequential/gru/gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2'
%sequential/gru/gru_cell/strided_sliceĎ
 sequential/gru/gru_cell/MatMul_1MatMulsequential/gru/zeros:output:0.sequential/gru/gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2"
 sequential/gru/gru_cell/MatMul_1
sequential/gru/gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2!
sequential/gru/gru_cell/Const_1Ą
)sequential/gru/gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2+
)sequential/gru/gru_cell/split_1/split_dimÉ
sequential/gru/gru_cell/split_1SplitV*sequential/gru/gru_cell/MatMul_1:product:0(sequential/gru/gru_cell/Const_1:output:02sequential/gru/gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2!
sequential/gru/gru_cell/split_1Ç
sequential/gru/gru_cell/addAddV2&sequential/gru/gru_cell/split:output:0(sequential/gru/gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/add
sequential/gru/gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2!
sequential/gru/gru_cell/Const_2
sequential/gru/gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2!
sequential/gru/gru_cell/Const_3ž
sequential/gru/gru_cell/MulMulsequential/gru/gru_cell/add:z:0(sequential/gru/gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/MulÂ
sequential/gru/gru_cell/Add_1Addsequential/gru/gru_cell/Mul:z:0(sequential/gru/gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/Add_1§
/sequential/gru/gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?21
/sequential/gru/gru_cell/clip_by_value/Minimum/yř
-sequential/gru/gru_cell/clip_by_value/MinimumMinimum!sequential/gru/gru_cell/Add_1:z:08sequential/gru/gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2/
-sequential/gru/gru_cell/clip_by_value/Minimum
'sequential/gru/gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2)
'sequential/gru/gru_cell/clip_by_value/yđ
%sequential/gru/gru_cell/clip_by_valueMaximum1sequential/gru/gru_cell/clip_by_value/Minimum:z:00sequential/gru/gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2'
%sequential/gru/gru_cell/clip_by_valueË
sequential/gru/gru_cell/add_2AddV2&sequential/gru/gru_cell/split:output:1(sequential/gru/gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/add_2
sequential/gru/gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2!
sequential/gru/gru_cell/Const_4
sequential/gru/gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2!
sequential/gru/gru_cell/Const_5Ä
sequential/gru/gru_cell/Mul_1Mul!sequential/gru/gru_cell/add_2:z:0(sequential/gru/gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/Mul_1Ä
sequential/gru/gru_cell/Add_3Add!sequential/gru/gru_cell/Mul_1:z:0(sequential/gru/gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/Add_3Ť
1sequential/gru/gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?23
1sequential/gru/gru_cell/clip_by_value_1/Minimum/yţ
/sequential/gru/gru_cell/clip_by_value_1/MinimumMinimum!sequential/gru/gru_cell/Add_3:z:0:sequential/gru/gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 21
/sequential/gru/gru_cell/clip_by_value_1/Minimum
)sequential/gru/gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2+
)sequential/gru/gru_cell/clip_by_value_1/yř
'sequential/gru/gru_cell/clip_by_value_1Maximum3sequential/gru/gru_cell/clip_by_value_1/Minimum:z:02sequential/gru/gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2)
'sequential/gru/gru_cell/clip_by_value_1Ă
sequential/gru/gru_cell/mul_2Mul+sequential/gru/gru_cell/clip_by_value_1:z:0sequential/gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/mul_2Ä
(sequential/gru/gru_cell/ReadVariableOp_1ReadVariableOp/sequential_gru_gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02*
(sequential/gru/gru_cell/ReadVariableOp_1Ż
-sequential/gru/gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2/
-sequential/gru/gru_cell/strided_slice_1/stackł
/sequential/gru/gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        21
/sequential/gru/gru_cell/strided_slice_1/stack_1ł
/sequential/gru/gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      21
/sequential/gru/gru_cell/strided_slice_1/stack_2
'sequential/gru/gru_cell/strided_slice_1StridedSlice0sequential/gru/gru_cell/ReadVariableOp_1:value:06sequential/gru/gru_cell/strided_slice_1/stack:output:08sequential/gru/gru_cell/strided_slice_1/stack_1:output:08sequential/gru/gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2)
'sequential/gru/gru_cell/strided_slice_1Ő
 sequential/gru/gru_cell/MatMul_2MatMul!sequential/gru/gru_cell/mul_2:z:00sequential/gru/gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 sequential/gru/gru_cell/MatMul_2Í
sequential/gru/gru_cell/add_4AddV2&sequential/gru/gru_cell/split:output:2*sequential/gru/gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/add_4
sequential/gru/gru_cell/TanhTanh!sequential/gru/gru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/TanhÁ
sequential/gru/gru_cell/mul_3Mul)sequential/gru/gru_cell/clip_by_value:z:0sequential/gru/zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/mul_3
sequential/gru/gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
sequential/gru/gru_cell/sub/xĆ
sequential/gru/gru_cell/subSub&sequential/gru/gru_cell/sub/x:output:0)sequential/gru/gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/subş
sequential/gru/gru_cell/mul_4Mulsequential/gru/gru_cell/sub:z:0 sequential/gru/gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/mul_4ż
sequential/gru/gru_cell/add_5AddV2!sequential/gru/gru_cell/mul_3:z:0!sequential/gru/gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
sequential/gru/gru_cell/add_5­
,sequential/gru/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2.
,sequential/gru/TensorArrayV2_1/element_shapeô
sequential/gru/TensorArrayV2_1TensorListReserve5sequential/gru/TensorArrayV2_1/element_shape:output:0'sequential/gru/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02 
sequential/gru/TensorArrayV2_1l
sequential/gru/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential/gru/time
'sequential/gru/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2)
'sequential/gru/while/maximum_iterations
!sequential/gru/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2#
!sequential/gru/while/loop_counterđ
sequential/gru/whileWhile*sequential/gru/while/loop_counter:output:00sequential/gru/while/maximum_iterations:output:0sequential/gru/time:output:0'sequential/gru/TensorArrayV2_1:handle:0sequential/gru/zeros:output:0'sequential/gru/strided_slice_1:output:0Fsequential/gru/TensorArrayUnstack/TensorListFromTensor:output_handle:06sequential_gru_gru_cell_matmul_readvariableop_resource7sequential_gru_gru_cell_biasadd_readvariableop_resource/sequential_gru_gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*+
body#R!
sequential_gru_while_body_14812*+
cond#R!
sequential_gru_while_cond_14811*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
sequential/gru/whileÓ
?sequential/gru/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2A
?sequential/gru/TensorArrayV2Stack/TensorListStack/element_shape­
1sequential/gru/TensorArrayV2Stack/TensorListStackTensorListStacksequential/gru/while:output:3Hsequential/gru/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype023
1sequential/gru/TensorArrayV2Stack/TensorListStack
$sequential/gru/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2&
$sequential/gru/strided_slice_3/stack
&sequential/gru/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2(
&sequential/gru/strided_slice_3/stack_1
&sequential/gru/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2(
&sequential/gru/strided_slice_3/stack_2ô
sequential/gru/strided_slice_3StridedSlice:sequential/gru/TensorArrayV2Stack/TensorListStack:tensor:0-sequential/gru/strided_slice_3/stack:output:0/sequential/gru/strided_slice_3/stack_1:output:0/sequential/gru/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2 
sequential/gru/strided_slice_3
sequential/gru/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2!
sequential/gru/transpose_1/permę
sequential/gru/transpose_1	Transpose:sequential/gru/TensorArrayV2Stack/TensorListStack:tensor:0(sequential/gru/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
sequential/gru/transpose_1
sequential/gru/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential/gru/runtimeŔ
&sequential/dense/MatMul/ReadVariableOpReadVariableOp/sequential_dense_matmul_readvariableop_resource*
_output_shapes

: *
dtype02(
&sequential/dense/MatMul/ReadVariableOpÇ
sequential/dense/MatMulMatMul'sequential/gru/strided_slice_3:output:0.sequential/dense/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
sequential/dense/MatMulż
'sequential/dense/BiasAdd/ReadVariableOpReadVariableOp0sequential_dense_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02)
'sequential/dense/BiasAdd/ReadVariableOpĹ
sequential/dense/BiasAddBiasAdd!sequential/dense/MatMul:product:0/sequential/dense/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2
sequential/dense/BiasAdd
IdentityIdentity!sequential/dense/BiasAdd:output:0^sequential/gru/while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2,
sequential/gru/whilesequential/gru/while:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
î
ń
E__inference_sequential_layer_call_and_return_conditional_losses_16030

inputs
	gru_16017
	gru_16019
	gru_16021
dense_16024
dense_16026
identity˘dense/StatefulPartitionedCall˘gru/StatefulPartitionedCallă
gru/StatefulPartitionedCallStatefulPartitionedCallinputs	gru_16017	gru_16019	gru_16021*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_157372
gru/StatefulPartitionedCallţ
dense/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0dense_16024dense_16026*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_dense_layer_call_and_return_conditional_losses_159782
dense/StatefulPartitionedCall¸
IdentityIdentity&dense/StatefulPartitionedCall:output:0^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ür
ć
>__inference_gru_layer_call_and_return_conditional_losses_15737

inputs+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_15626*
condR
while_cond_15625*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
í4
ć
C__inference_gru_cell_layer_call_and_return_conditional_losses_17471

inputs
states_0"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
readvariableop_resource
identity

identity_1
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:`*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split/split_dimŹ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
splitx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ü
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
strided_slicer
MatMul_1MatMulstates_0strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2

MatMul_1g
Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2	
Const_1q
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split_1/split_dimŃ
split_1SplitVMatMul_1:product:0Const_1:output:0split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
addW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3^
MulMuladd:z:0Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mulb
Add_1AddMul:z:0Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value/Minimum/y
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_valuek
add_2AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_2W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5d
Mul_1Mul	add_2:z:0Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mul_1d
Add_3Add	Mul_1:z:0Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value_1/Minimum/y
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1f
mul_2Mulclip_by_value_1:z:0states_0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_2|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stack_1
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_1u
MatMul_2MatMul	mul_2:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

MatMul_2m
add_4AddV2split:output:2MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Tanhd
mul_3Mulclip_by_value:z:0states_0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_3S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
subZ
mul_4Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_4_
add_5AddV2	mul_3:z:0	mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ ::::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:QM
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ź4
ď
__inference__traced_save_17638
file_prefix+
'savev2_dense_kernel_read_readvariableop)
%savev2_dense_bias_read_readvariableop+
'savev2_rmsprop_iter_read_readvariableop	,
(savev2_rmsprop_decay_read_readvariableop4
0savev2_rmsprop_learning_rate_read_readvariableop/
+savev2_rmsprop_momentum_read_readvariableop*
&savev2_rmsprop_rho_read_readvariableop2
.savev2_gru_gru_cell_kernel_read_readvariableop<
8savev2_gru_gru_cell_recurrent_kernel_read_readvariableop0
,savev2_gru_gru_cell_bias_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop7
3savev2_rmsprop_dense_kernel_rms_read_readvariableop5
1savev2_rmsprop_dense_bias_rms_read_readvariableop>
:savev2_rmsprop_gru_gru_cell_kernel_rms_read_readvariableopH
Dsavev2_rmsprop_gru_gru_cell_recurrent_kernel_rms_read_readvariableop<
8savev2_rmsprop_gru_gru_cell_bias_rms_read_readvariableop
savev2_1_const

identity_1˘MergeV2Checkpoints˘SaveV2˘SaveV2_1
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Const
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*<
value3B1 B+_temp_fc90d92b83ca422a92e56a8df079668c/part2	
Const_1
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shardŚ
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilenameľ
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*Ç
value˝BşB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE2
SaveV2/tensor_namesŞ
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slicesć
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0'savev2_dense_kernel_read_readvariableop%savev2_dense_bias_read_readvariableop'savev2_rmsprop_iter_read_readvariableop(savev2_rmsprop_decay_read_readvariableop0savev2_rmsprop_learning_rate_read_readvariableop+savev2_rmsprop_momentum_read_readvariableop&savev2_rmsprop_rho_read_readvariableop.savev2_gru_gru_cell_kernel_read_readvariableop8savev2_gru_gru_cell_recurrent_kernel_read_readvariableop,savev2_gru_gru_cell_bias_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop3savev2_rmsprop_dense_kernel_rms_read_readvariableop1savev2_rmsprop_dense_bias_rms_read_readvariableop:savev2_rmsprop_gru_gru_cell_kernel_rms_read_readvariableopDsavev2_rmsprop_gru_gru_cell_recurrent_kernel_rms_read_readvariableop8savev2_rmsprop_gru_gru_cell_bias_rms_read_readvariableop"/device:CPU:0*
_output_shapes
 *
dtypes
2	2
SaveV2
ShardedFilename_1/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B :2
ShardedFilename_1/shardŹ
ShardedFilename_1ShardedFilenameStringJoin:output:0 ShardedFilename_1/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename_1˘
SaveV2_1/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*1
value(B&B_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2_1/tensor_names
SaveV2_1/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*
valueB
B 2
SaveV2_1/shape_and_slicesĎ
SaveV2_1SaveV2ShardedFilename_1:filename:0SaveV2_1/tensor_names:output:0"SaveV2_1/shape_and_slices:output:0savev2_1_const^SaveV2"/device:CPU:0*
_output_shapes
 *
dtypes
22

SaveV2_1ă
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0ShardedFilename_1:filename:0^SaveV2	^SaveV2_1"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixesŹ
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix	^SaveV2_1"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identity

Identity_1IdentityIdentity:output:0^MergeV2Checkpoints^SaveV2	^SaveV2_1*
T0*
_output_shapes
: 2

Identity_1"!

identity_1Identity_1:output:0*y
_input_shapesh
f: : :: : : : : :`: `:`: : : ::`: `:`: 2(
MergeV2CheckpointsMergeV2Checkpoints2
SaveV2SaveV22
SaveV2_1SaveV2_1:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

: : 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:`:$	 

_output_shapes

: `: 


_output_shapes
:`:

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

: : 

_output_shapes
::$ 

_output_shapes

:`:$ 

_output_shapes

: `: 

_output_shapes
:`:

_output_shapes
: 
Í
Ź
*__inference_sequential_layer_call_fn_16528

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identity˘StatefulPartitionedCallú
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_sequential_layer_call_and_return_conditional_losses_160302
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 


Ş
(__inference_gru_cell_layer_call_fn_17560

inputs
states_0
unknown
	unknown_0
	unknown_1
identity

identity_1˘StatefulPartitionedCallý
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150832
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ :::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:QM
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ţ<
Ä
>__inference_gru_layer_call_and_return_conditional_losses_15406

inputs
gru_cell_15330
gru_cell_15332
gru_cell_15334
identity˘ gru_cell/StatefulPartitionedCall˘whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¸
 gru_cell/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_15330gru_cell_15332gru_cell_15334*
Tin	
2*
Tout
2*:
_output_shapes(
&:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*L
fGRE
C__inference_gru_cell_layer_call_and_return_conditional_losses_150222"
 gru_cell/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterŮ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_15330gru_cell_15332gru_cell_15334*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_15342*
condR
while_cond_15341*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime
IdentityIdentitystrided_slice_3:output:0!^gru_cell/StatefulPartitionedCall^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2D
 gru_cell/StatefulPartitionedCall gru_cell/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
är
č
>__inference_gru_layer_call_and_return_conditional_losses_17369
inputs_0+
'gru_cell_matmul_readvariableop_resource,
(gru_cell_biasadd_readvariableop_resource$
 gru_cell_readvariableop_resource
identity˘whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :č2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2î
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
TensorArrayV2/element_shape˛
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2ż
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeř
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ü
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*
shrink_axis_mask2
strided_slice_2¨
gru_cell/MatMul/ReadVariableOpReadVariableOp'gru_cell_matmul_readvariableop_resource*
_output_shapes

:`*
dtype02 
gru_cell/MatMul/ReadVariableOp 
gru_cell/MatMulMatMulstrided_slice_2:output:0&gru_cell/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/MatMul§
gru_cell/BiasAdd/ReadVariableOpReadVariableOp(gru_cell_biasadd_readvariableop_resource*
_output_shapes
:`*
dtype02!
gru_cell/BiasAdd/ReadVariableOpĽ
gru_cell/BiasAddBiasAddgru_cell/MatMul:product:0'gru_cell/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
gru_cell/BiasAddb
gru_cell/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
gru_cell/Const
gru_cell/split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split/split_dimĐ
gru_cell/splitSplit!gru_cell/split/split_dim:output:0gru_cell/BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split
gru_cell/ReadVariableOpReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp
gru_cell/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
gru_cell/strided_slice/stack
gru_cell/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice/stack_1
gru_cell/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2 
gru_cell/strided_slice/stack_2˛
gru_cell/strided_sliceStridedSlicegru_cell/ReadVariableOp:value:0%gru_cell/strided_slice/stack:output:0'gru_cell/strided_slice/stack_1:output:0'gru_cell/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
gru_cell/strided_slice
gru_cell/MatMul_1MatMulzeros:output:0gru_cell/strided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2
gru_cell/MatMul_1y
gru_cell/Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2
gru_cell/Const_1
gru_cell/split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
gru_cell/split_1/split_dimţ
gru_cell/split_1SplitVgru_cell/MatMul_1:product:0gru_cell/Const_1:output:0#gru_cell/split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
gru_cell/split_1
gru_cell/addAddV2gru_cell/split:output:0gru_cell/split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/addi
gru_cell/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_2i
gru_cell/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_3
gru_cell/MulMulgru_cell/add:z:0gru_cell/Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul
gru_cell/Add_1Addgru_cell/Mul:z:0gru_cell/Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_1
 gru_cell/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2"
 gru_cell/clip_by_value/Minimum/yź
gru_cell/clip_by_value/MinimumMinimumgru_cell/Add_1:z:0)gru_cell/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2 
gru_cell/clip_by_value/Minimumy
gru_cell/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value/y´
gru_cell/clip_by_valueMaximum"gru_cell/clip_by_value/Minimum:z:0!gru_cell/clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value
gru_cell/add_2AddV2gru_cell/split:output:1gru_cell/split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_2i
gru_cell/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2
gru_cell/Const_4i
gru_cell/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell/Const_5
gru_cell/Mul_1Mulgru_cell/add_2:z:0gru_cell/Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Mul_1
gru_cell/Add_3Addgru_cell/Mul_1:z:0gru_cell/Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Add_3
"gru_cell/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2$
"gru_cell/clip_by_value_1/Minimum/yÂ
 gru_cell/clip_by_value_1/MinimumMinimumgru_cell/Add_3:z:0+gru_cell/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2"
 gru_cell/clip_by_value_1/Minimum}
gru_cell/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell/clip_by_value_1/yź
gru_cell/clip_by_value_1Maximum$gru_cell/clip_by_value_1/Minimum:z:0#gru_cell/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/clip_by_value_1
gru_cell/mul_2Mulgru_cell/clip_by_value_1:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_2
gru_cell/ReadVariableOp_1ReadVariableOp gru_cell_readvariableop_resource*
_output_shapes

: `*
dtype02
gru_cell/ReadVariableOp_1
gru_cell/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2 
gru_cell/strided_slice_1/stack
 gru_cell/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell/strided_slice_1/stack_1
 gru_cell/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell/strided_slice_1/stack_2ž
gru_cell/strided_slice_1StridedSlice!gru_cell/ReadVariableOp_1:value:0'gru_cell/strided_slice_1/stack:output:0)gru_cell/strided_slice_1/stack_1:output:0)gru_cell/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell/strided_slice_1
gru_cell/MatMul_2MatMulgru_cell/mul_2:z:0!gru_cell/strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/MatMul_2
gru_cell/add_4AddV2gru_cell/split:output:2gru_cell/MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_4l
gru_cell/TanhTanhgru_cell/add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/Tanh
gru_cell/mul_3Mulgru_cell/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_3e
gru_cell/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
gru_cell/sub/x
gru_cell/subSubgru_cell/sub/x:output:0gru_cell/clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/sub~
gru_cell/mul_4Mulgru_cell/sub:z:0gru_cell/Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/mul_4
gru_cell/add_5AddV2gru_cell/mul_3:z:0gru_cell/mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
gru_cell/add_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    2
TensorArrayV2_1/element_shape¸
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0'gru_cell_matmul_readvariableop_resource(gru_cell_biasadd_readvariableop_resource gru_cell_readvariableop_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_17258*
condR
while_cond_17257*8
output_shapes'
%: : : : :˙˙˙˙˙˙˙˙˙ : : : : : *
parallel_iterations 2
whileľ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"˙˙˙˙    22
0TensorArrayV2Stack/TensorListStack/element_shapeń
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ *
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
˙˙˙˙˙˙˙˙˙2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permŽ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::2
whilewhile:^ Z
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ă4
ä
C__inference_gru_cell_layer_call_and_return_conditional_losses_15022

inputs

states"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
readvariableop_resource
identity

identity_1
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:`*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:`*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙`2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constm
split/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split/split_dimŹ
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*M
_output_shapes;
9:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2
splitx
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ü
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: @*

begin_mask*
end_mask2
strided_slicep
MatMul_1MatMulstatesstrided_slice:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙@2

MatMul_1g
Const_1Const*
_output_shapes
:*
dtype0*!
valueB"        ˙˙˙˙2	
Const_1q
split_1/split_dimConst*
_output_shapes
: *
dtype0*
valueB :
˙˙˙˙˙˙˙˙˙2
split_1/split_dimŃ
split_1SplitVMatMul_1:product:0Const_1:output:0split_1/split_dim:output:0*
T0*

Tlen0*K
_output_shapes9
7:˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ :˙˙˙˙˙˙˙˙˙ *
	num_split2	
split_1g
addAddV2split:output:0split_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
addW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3^
MulMuladd:z:0Const_2:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mulb
Add_1AddMul:z:0Const_3:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value/Minimum/y
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_valuek
add_2AddV2split:output:1split_1:output:1*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_2W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍĚL>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5d
Mul_1Mul	add_2:z:0Const_4:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Mul_1d
Add_3Add	Mul_1:z:0Const_5:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
clip_by_value_1/Minimum/y
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
clip_by_value_1d
mul_2Mulclip_by_value_1:z:0states*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_2|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stack_1
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_1u
MatMul_2MatMul	mul_2:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

MatMul_2m
add_4AddV2split:output:2MatMul_2:product:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
Tanhb
mul_3Mulclip_by_value:z:0states*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_3S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
subZ
mul_4Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
mul_4_
add_5AddV2	mul_3:z:0	mul_4:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:˙˙˙˙˙˙˙˙˙:˙˙˙˙˙˙˙˙˙ ::::O K
'
_output_shapes
:˙˙˙˙˙˙˙˙˙
 
_user_specified_nameinputs:OK
'
_output_shapes
:˙˙˙˙˙˙˙˙˙ 
 
_user_specified_namestates:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
÷
ô
E__inference_sequential_layer_call_and_return_conditional_losses_16011
	gru_input
	gru_15998
	gru_16000
	gru_16002
dense_16005
dense_16007
identity˘dense/StatefulPartitionedCall˘gru/StatefulPartitionedCallć
gru/StatefulPartitionedCallStatefulPartitionedCall	gru_input	gru_15998	gru_16000	gru_16002*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*G
fBR@
>__inference_gru_layer_call_and_return_conditional_losses_159382
gru/StatefulPartitionedCallţ
dense/StatefulPartitionedCallStatefulPartitionedCall$gru/StatefulPartitionedCall:output:0dense_16005dense_16007*
Tin
2*
Tout
2*'
_output_shapes
:˙˙˙˙˙˙˙˙˙*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_dense_layer_call_and_return_conditional_losses_159782
dense/StatefulPartitionedCall¸
IdentityIdentity&dense/StatefulPartitionedCall:output:0^dense/StatefulPartitionedCall^gru/StatefulPartitionedCall*
T0*'
_output_shapes
:˙˙˙˙˙˙˙˙˙2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙:::::2>
dense/StatefulPartitionedCalldense/StatefulPartitionedCall2:
gru/StatefulPartitionedCallgru/StatefulPartitionedCall:_ [
4
_output_shapes"
 :˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
#
_user_specified_name	gru_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: "ŻL
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*š
serving_defaultĽ
L
	gru_input?
serving_default_gru_input:0˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙9
dense0
StatefulPartitionedCall:0˙˙˙˙˙˙˙˙˙tensorflow/serving/predict:Ł
 
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
@_default_save_signature
A__call__
*B&call_and_return_all_conditional_losses"ě
_tf_keras_sequentialÍ{"class_name": "Sequential", "name": "sequential", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "config": {"name": "sequential", "layers": [{"class_name": "GRU", "config": {"name": "gru", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2, "reset_after": false}}, {"class_name": "Dense", "config": {"name": "dense", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}, "is_graph_network": true, "keras_version": "2.3.0-tf", "backend": "tensorflow", "model_config": {"class_name": "Sequential", "config": {"name": "sequential", "layers": [{"class_name": "GRU", "config": {"name": "gru", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2, "reset_after": false}}, {"class_name": "Dense", "config": {"name": "dense", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}}, "training_config": {"loss": "mae", "metrics": null, "weighted_metrics": null, "loss_weights": null, "sample_weight_mode": null, "optimizer_config": {"class_name": "RMSprop", "config": {"name": "RMSprop", "learning_rate": 0.0010000000474974513, "decay": 0.0, "rho": 0.8999999761581421, "momentum": 0.0, "epsilon": 1e-07, "centered": false}}}}

	cell


state_spec
	variables
trainable_variables
regularization_losses
	keras_api
C__call__
*D&call_and_return_all_conditional_losses"č

_tf_keras_rnn_layerĘ
{"class_name": "GRU", "name": "gru", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "stateful": false, "config": {"name": "gru", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2, "reset_after": false}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}
Ę

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
E__call__
*F&call_and_return_all_conditional_losses"Ľ
_tf_keras_layer{"class_name": "Dense", "name": "dense", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "config": {"name": "dense", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 32}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 32]}}

iter
	decay
learning_rate
momentum
rho	rms;	rms<	rms=	rms>	rms?"
	optimizer
C
0
1
2
3
4"
trackable_list_wrapper
C
0
1
2
3
4"
trackable_list_wrapper
 "
trackable_list_wrapper
Ę
metrics
non_trainable_variables

layers
 layer_regularization_losses
!layer_metrics
	variables
trainable_variables
regularization_losses
A__call__
@_default_save_signature
*B&call_and_return_all_conditional_losses
&B"call_and_return_conditional_losses"
_generic_user_object
,
Gserving_default"
signature_map
˙

kernel
recurrent_kernel
bias
"	variables
#trainable_variables
$regularization_losses
%	keras_api
H__call__
*I&call_and_return_all_conditional_losses"Ä
_tf_keras_layerŞ{"class_name": "GRUCell", "name": "gru_cell", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "config": {"name": "gru_cell", "trainable": true, "dtype": "float32", "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2, "reset_after": false}}
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
š
&metrics

'states
(non_trainable_variables

)layers
*layer_regularization_losses
+layer_metrics
	variables
trainable_variables
regularization_losses
C__call__
*D&call_and_return_all_conditional_losses
&D"call_and_return_conditional_losses"
_generic_user_object
: 2dense/kernel
:2
dense/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
,metrics
-non_trainable_variables

.layers
/layer_regularization_losses
0layer_metrics
	variables
trainable_variables
regularization_losses
E__call__
*F&call_and_return_all_conditional_losses
&F"call_and_return_conditional_losses"
_generic_user_object
:	 (2RMSprop/iter
: (2RMSprop/decay
: (2RMSprop/learning_rate
: (2RMSprop/momentum
: (2RMSprop/rho
%:#`2gru/gru_cell/kernel
/:- `2gru/gru_cell/recurrent_kernel
:`2gru/gru_cell/bias
'
10"
trackable_list_wrapper
 "
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
­
2metrics
3non_trainable_variables

4layers
5layer_regularization_losses
6layer_metrics
"	variables
#trainable_variables
$regularization_losses
H__call__
*I&call_and_return_all_conditional_losses
&I"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
	0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
ť
	7total
	8count
9	variables
:	keras_api"
_tf_keras_metricj{"class_name": "Mean", "name": "loss", "dtype": "float32", "config": {"name": "loss", "dtype": "float32"}}
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
:  (2total
:  (2count
.
70
81"
trackable_list_wrapper
-
9	variables"
_generic_user_object
(:& 2RMSprop/dense/kernel/rms
": 2RMSprop/dense/bias/rms
/:-`2RMSprop/gru/gru_cell/kernel/rms
9:7 `2)RMSprop/gru/gru_cell/recurrent_kernel/rms
):'`2RMSprop/gru/gru_cell/bias/rms
í2ę
 __inference__wrapped_model_14929Ĺ
˛
FullArgSpec
args 
varargsjargs
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsŞ *5˘2
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
ö2ó
*__inference_sequential_layer_call_fn_16528
*__inference_sequential_layer_call_fn_16043
*__inference_sequential_layer_call_fn_16074
*__inference_sequential_layer_call_fn_16543Ŕ
ˇ˛ł
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
â2ß
E__inference_sequential_layer_call_and_return_conditional_losses_16306
E__inference_sequential_layer_call_and_return_conditional_losses_15995
E__inference_sequential_layer_call_and_return_conditional_losses_16513
E__inference_sequential_layer_call_and_return_conditional_losses_16011Ŕ
ˇ˛ł
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
ď2ě
#__inference_gru_layer_call_fn_17391
#__inference_gru_layer_call_fn_16956
#__inference_gru_layer_call_fn_16967
#__inference_gru_layer_call_fn_17380Ő
Ě˛Č
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
Ű2Ř
>__inference_gru_layer_call_and_return_conditional_losses_17369
>__inference_gru_layer_call_and_return_conditional_losses_17168
>__inference_gru_layer_call_and_return_conditional_losses_16945
>__inference_gru_layer_call_and_return_conditional_losses_16744Ő
Ě˛Č
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
Ď2Ě
%__inference_dense_layer_call_fn_17410˘
˛
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsŞ *
 
ę2ç
@__inference_dense_layer_call_and_return_conditional_losses_17401˘
˛
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsŞ *
 
4B2
#__inference_signature_wrapper_16099	gru_input
2
(__inference_gru_cell_layer_call_fn_17546
(__inference_gru_cell_layer_call_fn_17560ž
ľ˛ą
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
Î2Ë
C__inference_gru_cell_layer_call_and_return_conditional_losses_17532
C__inference_gru_cell_layer_call_and_return_conditional_losses_17471ž
ľ˛ą
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsŞ 
annotationsŞ *
 
 __inference__wrapped_model_14929w?˘<
5˘2
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
Ş "-Ş*
(
dense
dense˙˙˙˙˙˙˙˙˙ 
@__inference_dense_layer_call_and_return_conditional_losses_17401\/˘,
%˘"
 
inputs˙˙˙˙˙˙˙˙˙ 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙
 x
%__inference_dense_layer_call_fn_17410O/˘,
%˘"
 
inputs˙˙˙˙˙˙˙˙˙ 
Ş "˙˙˙˙˙˙˙˙˙˙
C__inference_gru_cell_layer_call_and_return_conditional_losses_17471ˇ\˘Y
R˘O
 
inputs˙˙˙˙˙˙˙˙˙
'˘$
"
states/0˙˙˙˙˙˙˙˙˙ 
p
Ş "R˘O
H˘E

0/0˙˙˙˙˙˙˙˙˙ 
$!

0/1/0˙˙˙˙˙˙˙˙˙ 
 ˙
C__inference_gru_cell_layer_call_and_return_conditional_losses_17532ˇ\˘Y
R˘O
 
inputs˙˙˙˙˙˙˙˙˙
'˘$
"
states/0˙˙˙˙˙˙˙˙˙ 
p 
Ş "R˘O
H˘E

0/0˙˙˙˙˙˙˙˙˙ 
$!

0/1/0˙˙˙˙˙˙˙˙˙ 
 Ö
(__inference_gru_cell_layer_call_fn_17546Š\˘Y
R˘O
 
inputs˙˙˙˙˙˙˙˙˙
'˘$
"
states/0˙˙˙˙˙˙˙˙˙ 
p
Ş "D˘A

0˙˙˙˙˙˙˙˙˙ 
"

1/0˙˙˙˙˙˙˙˙˙ Ö
(__inference_gru_cell_layer_call_fn_17560Š\˘Y
R˘O
 
inputs˙˙˙˙˙˙˙˙˙
'˘$
"
states/0˙˙˙˙˙˙˙˙˙ 
p 
Ş "D˘A

0˙˙˙˙˙˙˙˙˙ 
"

1/0˙˙˙˙˙˙˙˙˙ ¸
>__inference_gru_layer_call_and_return_conditional_losses_16744vH˘E
>˘;
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙ 
 ¸
>__inference_gru_layer_call_and_return_conditional_losses_16945vH˘E
>˘;
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p 

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙ 
 ż
>__inference_gru_layer_call_and_return_conditional_losses_17168}O˘L
E˘B
41
/,
inputs/0˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙ 
 ż
>__inference_gru_layer_call_and_return_conditional_losses_17369}O˘L
E˘B
41
/,
inputs/0˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p 

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙ 
 
#__inference_gru_layer_call_fn_16956iH˘E
>˘;
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p

 
Ş "˙˙˙˙˙˙˙˙˙ 
#__inference_gru_layer_call_fn_16967iH˘E
>˘;
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p 

 
Ş "˙˙˙˙˙˙˙˙˙ 
#__inference_gru_layer_call_fn_17380pO˘L
E˘B
41
/,
inputs/0˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p

 
Ş "˙˙˙˙˙˙˙˙˙ 
#__inference_gru_layer_call_fn_17391pO˘L
E˘B
41
/,
inputs/0˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

 
p 

 
Ş "˙˙˙˙˙˙˙˙˙ Ŕ
E__inference_sequential_layer_call_and_return_conditional_losses_15995wG˘D
=˘:
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙
 Ŕ
E__inference_sequential_layer_call_and_return_conditional_losses_16011wG˘D
=˘:
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p 

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙
 ˝
E__inference_sequential_layer_call_and_return_conditional_losses_16306tD˘A
:˘7
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙
 ˝
E__inference_sequential_layer_call_and_return_conditional_losses_16513tD˘A
:˘7
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p 

 
Ş "%˘"

0˙˙˙˙˙˙˙˙˙
 
*__inference_sequential_layer_call_fn_16043jG˘D
=˘:
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p

 
Ş "˙˙˙˙˙˙˙˙˙
*__inference_sequential_layer_call_fn_16074jG˘D
=˘:
0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p 

 
Ş "˙˙˙˙˙˙˙˙˙
*__inference_sequential_layer_call_fn_16528gD˘A
:˘7
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p

 
Ş "˙˙˙˙˙˙˙˙˙
*__inference_sequential_layer_call_fn_16543gD˘A
:˘7
-*
inputs˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙
p 

 
Ş "˙˙˙˙˙˙˙˙˙Ź
#__inference_signature_wrapper_16099L˘I
˘ 
BŞ?
=
	gru_input0-
	gru_input˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙"-Ş*
(
dense
dense˙˙˙˙˙˙˙˙˙