

import os

if (os.getenv('KERAS_IMPLEMENTATION', 'keras') == 'tensorflow'):
  from tensorflow.python.keras.callbacks import Callback
else:
  from keras.callbacks import Callback

class RCallback(Callback):
  
  def __init__(self, r_set_context,
                     r_on_epoch_begin,
                     r_on_epoch_end,
                     r_on_batch_begin,
                     r_on_batch_end,
                     r_on_train_begin,
                     r_on_train_end):
    super(Callback, self).__init__()
    self.r_set_context = r_set_context
    self.r_on_epoch_begin = r_on_epoch_begin
    self.r_on_epoch_end = r_on_epoch_end
    self.r_on_batch_begin = r_on_batch_begin
    self.r_on_batch_end = r_on_batch_end
    self.r_on_train_begin = r_on_train_begin
    self.r_on_train_end = r_on_train_end
  
  def on_epoch_begin(self, epoch, logs=None):
    self.r_on_epoch_begin(epoch, logs)

  def on_epoch_end(self, epoch, logs=None):
    self.r_on_epoch_end(epoch, logs)

  def on_batch_begin(self, batch, logs=None):
    self.r_on_batch_begin(batch, logs)
 
  def on_batch_end(self, batch, logs=None):
    self.r_on_batch_end(batch, logs)

  def on_train_begin(self, logs=None):
    self.r_set_context(self.params, self.model)
    self.r_on_train_begin(logs)
 
  def on_train_end(self, logs=None):
    self.r_on_train_end(logs)
 

 
