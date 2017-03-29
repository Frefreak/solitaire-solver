from keras.models import Sequential, load_model
from keras.layers import Dense, Dropout, Flatten
from keras.layers.convolutional import Conv2D, MaxPooling2D
from keras.preprocessing.image import ImageDataGenerator
from keras.optimizers import Adam

label_file = '../data/label_list.txt'
train_dir = './train_data'
test_dir = './test_data'
with open(label_file, 'r') as f:
    labels = f.read().splitlines() + ['none']

# model = load_model('./final_model.h5')
img_size = 24
model = Sequential()
model.add(Conv2D(48, (3, 3), strides=1, padding='same', \
                activation='relu', input_shape=(img_size, img_size, 3)))
model.add(Conv2D(64, (3, 3), strides=1, padding='same', \
                activation='relu', input_shape=(img_size, img_size, 3)))
model.add(Conv2D(128, (3, 3), strides=1, padding='same', \
                activation='relu', input_shape=(img_size, img_size, 3)))
model.add(MaxPooling2D())
model.add(Dropout(0.25))

model.add(Flatten())
model.add(Dense(512, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(len(labels), activation='softmax'))

opt = Adam()
model.compile(loss='categorical_crossentropy', optimizer=opt, \
        metrics=['accuracy'])

train_generator = ImageDataGenerator(rotation_range=22, \
        width_shift_range=0.21, height_shift_range=0.21, rescale=1./255)
test_generator = ImageDataGenerator(rescale=1./255)
train_gen = train_generator.flow_from_directory(train_dir, \
        target_size=(img_size, img_size), classes=labels)
test_gen = test_generator.flow_from_directory(test_dir, \
        target_size=(img_size, img_size), classes=labels)

model.fit_generator( \
        train_gen, \
        steps_per_epoch=500, \
        epochs=40, \
        validation_data=test_gen, \
        validation_steps=500)

model.save('final_model.h5')
